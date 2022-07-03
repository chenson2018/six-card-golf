run-dev: elm rust

elm:
	make -C frontend

rust:
	cargo run --manifest-path=backend/Cargo.toml
