package io.choerodon.agile.api.controller.v2;

import com.alibaba.fastjson.JSONObject;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.app.service.BoardService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-12-06
 */
@RestController
@RequestMapping(value = "/v2/projects/{project_id}/board")
public class BoardV2Controller {

    @Autowired
    private BoardService boardService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("all data , Refactoring")
    @PostMapping(value = "/{boardId}/all_data/{organization_id}")
    public ResponseEntity<JSONObject> queryByOptions(@ApiParam(value = "项目id", required = true)
                                                     @PathVariable(name = "project_id") Long projectId,
                                                     @ApiParam(value = "agile board id", required = true)
                                                     @PathVariable @Encrypt Long boardId,
                                                     @ApiParam(value = "组织id", required = true)
                                                     @PathVariable(name = "organization_id") Long organizationId,
                                                     @ApiParam(value = "筛选条件")
                                                     @RequestBody SearchParamVO searchParamVO) {
        return Results.success(boardService.queryAllDataV2(projectId, boardId, organizationId, searchParamVO));
    }
}
