package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.DataLogCreateVO;
import io.choerodon.agile.api.vo.DataLogQueryVO;
import io.choerodon.agile.api.vo.business.AllDataLogVO;
import io.choerodon.agile.api.vo.business.DataLogVO;
import io.choerodon.agile.app.service.DataLogService;

import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.exception.CommonException;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import java.util.List;
import java.util.Optional;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/6/14.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/data_log")
public class DataLogController {

    @Autowired
    private DataLogService dataLogService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建DataLog")
    @PostMapping
    public ResponseEntity<DataLogVO> createDataLog(@ApiParam(value = "项目id", required = true)
                                                     @PathVariable(name = "project_id") Long projectId,
                                                   @ApiParam(value = "data log object", required = true)
                                                     @RequestBody DataLogCreateVO createVO) {
        return Optional.ofNullable(dataLogService.createDataLog(projectId, createVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.dataLog.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询DataLog列表")
    @GetMapping
    public ResponseEntity<List<DataLogVO>> listByIssueId(@ApiParam(value = "项目id", required = true)
                                                          @PathVariable(name = "project_id") Long projectId,
                                                         @ApiParam(value = "issue id", required = true)
                                                          @RequestParam @Encrypt Long issueId) {
        return Optional.ofNullable(dataLogService.listByIssueId(projectId, issueId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.dataLogList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询操作动态")
    @PostMapping("/all")
    public ResponseEntity<Page<AllDataLogVO>> test(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "查询参数")
            @RequestBody DataLogQueryVO dataLogQueryVO,
            @ApiParam(value = "分页信息", required = true)
            @SortDefault(sort = "creationDate", direction = Sort.Direction.DESC)
            @ApiIgnore PageRequest pageRequest) {
        return Optional.ofNullable(dataLogService.listAllDataLogByProjectId(projectId, dataLogQueryVO, pageRequest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.dataLogList.get"));
    }
}
