run-dev: elm rust-run

elm:
	make -C frontend

rust-run:
	cargo run --manifest-path=backend/Cargo.toml
