package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.StatusLinkageVO;
import io.choerodon.agile.app.service.StatusLinkageService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2020-08-17 19:47
 */
@RestController
@RequestMapping("/v1/projects/{project_id}/status_linkages")
public class StatusLinkageController {
    @Autowired
    private StatusLinkageService statusLinkageService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("为状态配置父子级联动")
    @PostMapping
    public ResponseEntity<List<StatusLinkageVO>> createOrUpdate(@ApiParam(value = "项目id", required = true)
                                                                     @PathVariable(name = "project_id") Long projectId,
                                                                @RequestParam @Encrypt Long issueTypeId,
                                                                @RequestParam @Encrypt Long statusId,
                                                                @RequestParam Long objectVersionNumber,
                                                                @RequestParam String applyType,
                                                                @RequestBody List<StatusLinkageVO> list) {
        return Optional.ofNullable(statusLinkageService.createOrUpdate(projectId, issueTypeId, statusId, objectVersionNumber, applyType, list))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.status.linkage.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询")
    @GetMapping("/list")
    public ResponseEntity<List<StatusLinkageVO>> listStatusLinkage(@ApiParam(value = "项目id", required = true)
                                                                       @PathVariable(name = "project_id") Long projectId,
                                                                       @RequestParam @Encrypt Long issueTypeId,
                                                                       @RequestParam @Encrypt Long statusId) {
        return Optional.ofNullable(statusLinkageService.listByIssueTypeAndStatusId(projectId, issueTypeId, statusId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.status.linkage.get"));
    }
}
