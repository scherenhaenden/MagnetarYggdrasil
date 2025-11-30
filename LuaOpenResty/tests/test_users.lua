local describe = describe
local it = it
local assert = assert
local spy = require("luassert.spy")
local mock = require("luassert.mock")

describe("User Service", function()
    local user_service = require("app.services.user_service")
    local user_repo = require("app.repositories.user_repository")

    it("should create a user", function()
        local mock_repo = mock(user_repo, true)
        mock_repo.create.returns({id=1, username="test", email="test@test.com"}, nil)

        local user, err = user_service.create_user({username="test", email="test@test.com"})

        assert.are.same({id=1, username="test", email="test@test.com"}, user)
        assert.is_nil(err)
        assert.spy(mock_repo.create).was.called_with("test", "test@test.com")

        mock.revert(mock_repo)
    end)

    it("should fail to create user without username", function()
        local user, err = user_service.create_user({email="test@test.com"})
        assert.is_nil(user)
        assert.are.equal("Missing username or email", err)
    end)
end)
