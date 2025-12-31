// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::fs;
use std::io::Write;
use std::process::Command;
use serde::{Deserialize, Serialize};
use tauri::command;

#[derive(Debug, Serialize, Deserialize)]
pub struct AeroCoefficient {
    alpha: f64,
    cd: f64,
    cl: f64,
    cm: f64,
    cn: f64,
    ca: f64,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DatcomCase {
    case_id: String,
    mach: f64,
    reynolds: f64,
    coefficients: Vec<AeroCoefficient>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DatcomResults {
    cases: Vec<DatcomCase>,
    raw_output: String,
}

fn parse_datcom_output(output: &str) -> Vec<AeroCoefficient> {
    let mut coefficients = Vec::new();
    let lines: Vec<&str> = output.lines().collect();

    for (i, line) in lines.iter().enumerate() {
        // Look for the coefficient table header
        if line.contains("ALPHA") && line.contains("CD") && line.contains("CL") && line.contains("CM") {
            // Parse the data rows that follow
            for j in (i + 2)..lines.len() {
                let data_line = lines[j].trim();
                if data_line.is_empty() || data_line.starts_with("0***") || data_line.starts_with("1 ") {
                    break;
                }

                let parts: Vec<&str> = data_line.split_whitespace().collect();
                if parts.len() >= 6 {
                    if let (Ok(alpha), Ok(cd), Ok(cl), Ok(cm), Ok(cn), Ok(ca)) = (
                        parts[0].parse::<f64>(),
                        parts[1].parse::<f64>(),
                        parts[2].parse::<f64>(),
                        parts[3].parse::<f64>(),
                        parts[4].parse::<f64>(),
                        parts[5].parse::<f64>(),
                    ) {
                        coefficients.push(AeroCoefficient {
                            alpha,
                            cd,
                            cl,
                            cm,
                            cn,
                            ca,
                        });
                    }
                }
            }
            break;
        }
    }

    coefficients
}

#[command]
fn run_datcom(input_content: String) -> Result<DatcomResults, String> {
    // Get home directory
    let home = std::env::var("HOME").map_err(|e| format!("Failed to get HOME: {}", e))?;
    let datcom_path = format!("{}/datcom/datcom", home);
    let work_dir = format!("{}/datcom/work", home);

    // Create work directory if it doesn't exist
    fs::create_dir_all(&work_dir).map_err(|e| format!("Failed to create work dir: {}", e))?;

    // Write input file
    let input_path = format!("{}/input.inp", work_dir);
    let mut file = fs::File::create(&input_path)
        .map_err(|e| format!("Failed to create input file: {}", e))?;
    file.write_all(input_content.as_bytes())
        .map_err(|e| format!("Failed to write input file: {}", e))?;

    // Run DATCOM
    let output = Command::new(&datcom_path)
        .current_dir(&work_dir)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .and_then(|mut child| {
            if let Some(ref mut stdin) = child.stdin {
                stdin.write_all(b"input.inp\n")?;
            }
            child.wait_with_output()
        })
        .map_err(|e| format!("Failed to run DATCOM: {}", e))?;

    // Read output file
    let output_path = format!("{}/datcom.out", work_dir);
    let raw_output = fs::read_to_string(&output_path)
        .unwrap_or_else(|_| String::from("No output file generated"));

    // Parse the output
    let coefficients = parse_datcom_output(&raw_output);

    let case = DatcomCase {
        case_id: String::from("DATCOM UI Analysis"),
        mach: 0.6,  // TODO: Extract from output
        reynolds: 4.28e6,
        coefficients,
    };

    Ok(DatcomResults {
        cases: vec![case],
        raw_output,
    })
}

#[command]
fn get_datcom_path() -> Result<String, String> {
    let home = std::env::var("HOME").map_err(|e| format!("Failed to get HOME: {}", e))?;
    let datcom_path = format!("{}/datcom/datcom", home);

    if std::path::Path::new(&datcom_path).exists() {
        Ok(datcom_path)
    } else {
        Err(String::from("DATCOM not found. Please install it first."))
    }
}

fn main() {
    tauri::Builder::default()
        .invoke_handler(tauri::generate_handler![run_datcom, get_datcom_path])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
