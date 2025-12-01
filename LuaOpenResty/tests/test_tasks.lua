local describe = describe
local it = it
local assert = assert
local spy = require("luassert.spy")
local mock = require("luassert.mock")

describe("Task Service", function()
    local task_service = require("app.services.task_service")
    local task_repo = require("app.repositories.task_repository")
    local user_repo = require("app.repositories.user_repository")

    it("should create a task for existing user", function()
        local mock_task_repo = mock(task_repo, true)
        local mock_user_repo = mock(user_repo, true)

        mock_user_repo.find_by_id.returns({id=1})
        mock_task_repo.create.returns({id=1, user_id=1, title="T", description="D", done=false}, nil)

        local task, err = task_service.create_task(1, {title="T", description="D"})

        assert.are.same({id=1, user_id=1, title="T", description="D", done=false}, task)
        assert.is_nil(err)

        mock.revert(mock_task_repo)
        mock.revert(mock_user_repo)
    end)

    it("should fail to create task if user not found", function()
         local mock_user_repo = mock(user_repo, true)
         mock_user_repo.find_by_id.returns(nil)

         local task, err = task_service.create_task(999, {title="T", description="D"})

         assert.is_nil(task)
         assert.are.equal("User not found", err)

         mock.revert(mock_user_repo)
    end)
end)
