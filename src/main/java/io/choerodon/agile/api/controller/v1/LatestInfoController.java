package io.choerodon.agile.api.controller.v1;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import java.util.Optional;

import javax.validation.Valid;

import io.choerodon.agile.api.vo.DataLogQueryVO;
import io.choerodon.agile.api.vo.business.AllDataLogVO;
import io.choerodon.agile.app.service.LatestInfoService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author chihao.ran@hand-china.com
 * @since 2021-03-23
 */
@RestController
@RequestMapping(value = "/v1/latest_info")
public class LatestInfoController {

    @Autowired
    private LatestInfoService latestInfoService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询项目概览操作动态")
    @PostMapping("/project/{project_id}/operation")
    public ResponseEntity<Page<AllDataLogVO>> listLatestOperationInfoByProjectId(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "查询参数")
            @RequestBody @Valid DataLogQueryVO dataLogQueryVO,
            @ApiParam(value = "分页信息", required = true)
            @SortDefault(sort = "creationDate", direction = Sort.Direction.DESC)
            @ApiIgnore PageRequest pageRequest) {
        return Optional.ofNullable(latestInfoService.listLatestOperationInfoByProjectId(projectId, dataLogQueryVO, pageRequest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.dataLogList.get"));
    }
}
