using SearchLight, SearchLightSQLite

function up()
  SearchLight.Migration.create_table(:users) do
    [
      SearchLight.Migration.column(:id, :integer, primary_key = true)
      SearchLight.Migration.column(:username, :string, unique = true)
      SearchLight.Migration.column(:email, :string, unique = true)
    ]
  end

  SearchLight.Migration.create_table(:tasks) do
    [
      SearchLight.Migration.column(:id, :integer, primary_key = true)
      SearchLight.Migration.column(:user_id, :integer)
      SearchLight.Migration.column(:title, :string)
      SearchLight.Migration.column(:description, :string)
      SearchLight.Migration.column(:done, :boolean, default = false)
    ]
  end

  # SearchLight doesn't support raw SQL easily in migration DSL for foreign keys in SQLite sometimes,
  # but we can execute arbitrary SQL if needed. For now assuming DSL handles it or we enforce via app logic/constraints.
  # SQLite foreign key support requires PRAGMA foreign_keys = ON at connection time.
end

function down()
  SearchLight.Migration.drop_table(:tasks)
  SearchLight.Migration.drop_table(:users)
end
