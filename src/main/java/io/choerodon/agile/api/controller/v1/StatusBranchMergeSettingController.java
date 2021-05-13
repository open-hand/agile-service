package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.StatusBranchMergeSettingVO;
import io.choerodon.agile.app.service.StatusBranchMergeSettingService;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * @author superlee
 * @since 2021-04-19
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/status_branch_merge_setting")
public class StatusBranchMergeSettingController {

    @Autowired
    private StatusBranchMergeSettingService statusBranchMergeSettingService;

    @ApiOperation(value = "分支合并状态流转配置")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/issue_type/{issue_type_id}/status/{status_id}")
    public ResponseEntity<StatusBranchMergeSettingVO> query(@PathVariable("project_id") Long projectId,
                                                            @PathVariable("issue_type_id") @Encrypt Long issueTypeId,
                                                            @PathVariable("status_id") @Encrypt Long statusId) {
        return Results.success(statusBranchMergeSettingService.query(projectId, ConvertUtil.getOrganizationId(projectId), issueTypeId, statusId));
    }

    @ApiOperation(value = "分支合并状态流转配置")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PutMapping("/issue_type/{issue_type_id}/status/{status_id}/update_auto_transform")
    public ResponseEntity updateAutoTransform(@PathVariable("project_id") Long projectId,
                                                                          @PathVariable("issue_type_id") @Encrypt Long issueTypeId,
                                                                          @PathVariable("status_id") @Encrypt Long statusId,
                                                                          @RequestParam Boolean autoTransform) {
        statusBranchMergeSettingService.updateAutoTransform(projectId, ConvertUtil.getOrganizationId(projectId), issueTypeId, statusId, autoTransform);
        return Results.success();
    }

}
