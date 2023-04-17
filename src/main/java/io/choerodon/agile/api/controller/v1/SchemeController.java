package io.choerodon.agile.api.controller.v1;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.core.base.BaseController;
import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.hzero.starter.keyencrypt.core.IEncryptionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.PriorityService;
import io.choerodon.agile.app.service.ProjectConfigService;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.agile.infra.utils.ProjectUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;

/**
 * 根据项目id获取对应数据
 *
 * @author shinan.chen 2018/10/24
 */

@RestController
@RequestMapping(value = "/v1/projects/{project_id}")
public class SchemeController extends BaseController {

    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private ProjectUtil projectUtil;
    @Autowired
    private IEncryptionService encryptionService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目的问题类型列表")
    @GetMapping(value = "/schemes/query_issue_types")
    public ResponseEntity<List<IssueTypeVO>> queryIssueTypesByProjectId(@ApiParam(value = "项目id", required = true)
                                                                        @PathVariable("project_id") Long projectId,
                                                                        @ApiParam(value = "应用类型", required = true)
                                                                        @RequestParam("apply_type") String applyType,
                                                                        @ApiParam(value = "是否只查询启用的")
                                                                        @RequestParam(value = "only_enabled", defaultValue = "false",
                                                                                required = false) Boolean onlyEnabled) {
        return Results.success(projectConfigService.queryIssueTypesByProjectId(projectId, applyType, onlyEnabled));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目的问题类型列表，带对应的状态机id")
    @GetMapping(value = "/schemes/query_issue_types_with_sm_id")
    public ResponseEntity<List<IssueTypeWithStateMachineIdVO>> queryIssueTypesWithStateMachineIdByProjectId(@ApiParam(value = "项目id", required = true)
                                                                                                                @PathVariable("project_id") Long projectId,
                                                                                                            @ApiParam(value = "查询信息的项目id")
                                                                                                                @RequestParam(name = "target_project_id", required = false) Long targetProjectId,
                                                                                                            @ApiParam(value = "应用编码")
                                                                                                                @RequestParam(value = "apply_type", required = false) String applyType,
                                                                                                            @ApiParam(value = "是否只查询启用的")
                                                                                                                @RequestParam(value = "only_enabled", defaultValue = "false",
                                                                                                                        required = false) Boolean onlyEnabled) {
        return Results.success(projectConfigService.queryIssueTypesWithStateMachineIdByProjectId(projectId, targetProjectId, applyType, onlyEnabled));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下某个问题类型拥有的转换（包含可以转换到的状态）")
    @GetMapping(value = "/schemes/query_transforms")
    public ResponseEntity<List<TransformVO>> queryTransformsByProjectId(@ApiParam(value = "项目id", required = true)
                                                                        @PathVariable("project_id") Long projectId,
                                                                        @ApiParam(value = "当前状态id", required = true)
                                                                        @RequestParam("current_status_id") @Encrypt Long currentStatusId,
                                                                        @ApiParam(value = "问题id", required = true)
                                                                        @RequestParam("issue_id") @Encrypt Long issueId,
                                                                        @ApiParam(value = "问题类型id", required = true)
                                                                        @RequestParam("issue_type_id") @Encrypt Long issueTypeId,
                                                                        @ApiParam(value = "应用类型", required = true)
                                                                        @RequestParam("apply_type") String applyType) {
        return Results.success(projectConfigService.queryTransformsByProjectId(projectId, currentStatusId, issueId, issueTypeId, applyType));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下所有问题类型所有状态对应的转换")
    @GetMapping(value = "/schemes/query_transforms_map")
    public ResponseEntity<Map<String, Map<String, List<TransformVO>>>> queryTransformsMapByProjectId(@ApiParam(value = "项目id", required = true)
                                                                                                     @PathVariable("project_id") Long projectId,
                                                                                                     @ApiParam(value = "看版id")
                                                                                                     @RequestParam(required = false) @Encrypt Long boardId,
                                                                                                     @ApiParam(value = "应用类型", required = true)
                                                                                                     @RequestParam("apply_type") String applyType) {

        return Results.success(EncryptionUtils.encryptMapValueMap(projectConfigService.queryTransformsMapByProjectId(projectId, boardId, applyType)));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下某个问题类型的所有状态")
    @GetMapping(value = "/schemes/query_status_by_issue_type_id")
    public ResponseEntity<List<StatusVO>> queryStatusByIssueTypeId(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable("project_id") Long projectId,
                                                                   @ApiParam(value = "问题类型id", required = true)
                                                                   @RequestParam("issue_type_id") @Encrypt Long issueTypeId,
                                                                   @ApiParam(value = "应用类型", required = true)
                                                                   @RequestParam("apply_type") String applyType) {
        return Results.success(projectConfigService.queryStatusByIssueTypeId(projectId, issueTypeId, applyType));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下的所有状态")
    @GetMapping(value = "/schemes/query_status_by_project_id")
    public ResponseEntity<List<StatusVO>> queryStatusByProjectId(@ApiParam(value = "项目id", required = true)
                                                                     @PathVariable("project_id") Long projectId,
                                                                 @ApiParam(value = "查询信息的项目id")
                                                                     @RequestParam(name = "target_project_id", required = false) Long targetProjectId,
                                                                 @ApiParam(value = "应用类型")
                                                                     @RequestParam(value = "apply_type", required = false) String applyType) {
        return Results.success(projectConfigService.queryStatusByProjectId(projectId, targetProjectId, applyType));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目的问题类型对应的状态机id")
    @GetMapping(value = "/schemes/query_state_machine_id")
    public ResponseEntity<Long> queryStateMachineId(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable("project_id") Long projectId,
                                                    @ApiParam(value = "应用类型", required = true)
                                                    @RequestParam("apply_type") String applyType,
                                                    @ApiParam(value = "问题类型id", required = true)
                                                    @RequestParam("issue_type_id") @Encrypt Long issueTypeId) {
        return Results.success(projectConfigService.queryStateMachineId(projectId, applyType, issueTypeId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "【敏捷】新增状态")
    @PostMapping(value = "/schemes/create_status_for_agile")
    public ResponseEntity<StatusVO> createStatusForAgile(@ApiParam(value = "项目id", required = true)
                                                         @PathVariable("project_id") Long projectId,
                                                         @ApiParam(value = "应用类型", required = true)
                                                         @RequestParam String applyType,
                                                         @ApiParam(value = "状态", required = true)
                                                         @RequestBody StatusVO statusVO) {
        return Results.success(projectConfigService.createStatusForAgile(projectId, applyType, statusVO));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "【敏捷】校验是否能新增状态")
    @GetMapping(value = "/schemes/check_create_status_for_agile")
    public ResponseEntity<Boolean> checkCreateStatusForAgile(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable("project_id") Long projectId,
                                                             @ApiParam(value = "应用类型", required = true)
                                                             @RequestParam String applyType) {
        return Results.success((Boolean) projectConfigService.checkCreateStatusForAgile(projectId, applyType).get("flag"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "【敏捷】校验是否能删除状态")
    @GetMapping(value = "/schemes/check_remove_status_for_agile")
    public ResponseEntity<Boolean> checkRemoveStatusForAgile(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable("project_id") Long projectId,
                                                             @ApiParam(value = "状态id", required = true)
                                                             @RequestParam("status_id") @Encrypt Long statusId,
                                                             @ApiParam(value = "应用类型", required = true)
                                                             @RequestParam String applyType) {
        return Results.success(projectConfigService.checkRemoveStatusForAgile(projectId, statusId, applyType));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据项目id查询组织默认优先级")
    @GetMapping("/priority/default")
    public ResponseEntity<PriorityVO> queryDefaultByOrganizationId(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable("project_id") Long projectId) {
        Long organizationId = projectUtil.getOrganizationId(projectId);
        return Optional.ofNullable(priorityService.queryDefaultByOrganizationId(organizationId))
                .map(Results::success)
                .orElseThrow(() -> new CommonException("error.priority.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据项目id查询组织优先级列表")
    @GetMapping("/priority/list_by_org")
    public ResponseEntity<List<PriorityVO>> queryByOrganizationIdList(@ApiParam(value = "项目id", required = true)
                                                                      @PathVariable("project_id") Long projectId) {
        Long organizationId = projectUtil.getOrganizationId(projectId);
        return Optional.ofNullable(priorityService.queryByOrganizationIdList(organizationId))
                .map(Results::success)
                .orElseThrow(() -> new CommonException("error.priorityList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询工作流第一个状态")
    @GetMapping("/status/query_first_status")
    public ResponseEntity<String> queryWorkFlowFirstStatus(@ApiParam(value = "项目id", required = true)
                                                           @PathVariable("project_id") Long projectId,
                                                           @ApiParam(value = "applyType", required = true)
                                                           @RequestParam String applyType,
                                                           @ApiParam(value = "issueTypeId", required = true)
                                                           @RequestParam @Encrypt Long issueTypeId,
                                                           @ApiParam(value = "organizationId", required = true)
                                                           @RequestParam Long organizationId) {
        return Optional.ofNullable(projectConfigService.queryWorkFlowFirstStatus(projectId, applyType, issueTypeId, organizationId))
                .map(initStatusId -> encryptionService.encrypt(initStatusId.toString(), EncryptionUtils.BLANK_KEY))
                .map(Results::success)
                .orElseThrow(() -> new CommonException("error.firstStatus.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "状态与流转列表")
    @GetMapping(value = "/status_transform/list")
    public ResponseEntity<List<StatusAndTransformVO>> statusTransformList(@ApiParam(value = "项目id", required = true)
                                                                          @PathVariable("project_id") Long projectId,
                                                                          @ApiParam(value = "问题类型id", required = true)
                                                                          @RequestParam @Encrypt Long issueTypeId,
                                                                          @ApiParam(value = "应用类型", required = true)
                                                                          @RequestParam String applyType) {
        return Results.success(projectConfigService.statusTransformList(projectId, issueTypeId, applyType));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "支持对状态机-状态与流转进行排序")
    @PutMapping(value = "/status_transform/sort")
    public ResponseEntity<NodeSortVO> updateSort(@ApiParam(value = "项目id", required = true)
                                                 @PathVariable("project_id") Long projectId,
                                                 @ApiParam(value = "状态机id", required = true)
                                                 @Encrypt Long statusMachineId,
                                                 @ApiParam(value = "节点排序", required = true)
                                                 @RequestBody NodeSortVO nodeSortVO,
                                                 @ApiParam(value = "应用累心", required = true)
                                                 @RequestParam String applyType) {
        return Results.success(projectConfigService.updateSort(projectId, statusMachineId, nodeSortVO, applyType));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "设置状态机的默认状态")
    @PutMapping(value = "/status_transform/setting_default_status")
    public ResponseEntity<Void> settingDefaultStatus(@ApiParam(value = "项目id", required = true)
                                                     @PathVariable("project_id") Long projectId,
                                                     @ApiParam(value = "问题类型id", required = true)
                                                     @RequestParam @Encrypt Long issueTypeId,
                                                     @ApiParam(value = "状态机id", required = true)
                                                     @RequestParam @Encrypt Long stateMachineId,
                                                     @ApiParam(value = "状态id", required = true)
                                                     @RequestParam @Encrypt Long statusId) {
        projectConfigService.defaultStatus(projectId, issueTypeId, stateMachineId, statusId);
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "改变问题类型的转换")
    @PutMapping(value = "/status_transform/update")
    public ResponseEntity<List<StateMachineTransformUpdateVO>> updateTransformByIssueTypeId(@ApiParam(value = "项目id", required = true)
                                                                                            @PathVariable("project_id") Long projectId,
                                                                                            @ApiParam(value = "问题类型id", required = true)
                                                                                            @RequestParam @Encrypt Long issueTypeId,
                                                                                            @ApiParam(value = "应用类型", required = true)
                                                                                            @RequestParam String applyType,
                                                                                            @ApiParam(value = "问题类型转换", required = true)
                                                                                            @RequestBody List<StateMachineTransformUpdateVO> list) {

        return Results.success(projectConfigService.updateTransformByIssueTypeId(projectId, issueTypeId, applyType, list));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "添加新状态")
    @PostMapping(value = "/status/create")
    public ResponseEntity<StatusVO> createStatus(@ApiParam(value = "项目id", required = true)
                                                 @PathVariable("project_id") Long projectId,
                                                 @ApiParam(value = "问题类型id集合", required = true)
                                                 @RequestParam @Encrypt List<Long> issueTypeIds,
                                                 @ApiParam(value = "应用类型")
                                                 @RequestParam(required = false) String applyType,
                                                 @ApiParam(value = "状态", required = true)
                                                 @RequestBody StatusVO statusVO) {

        return Results.success(projectConfigService.createStatus(projectId, issueTypeIds, applyType, statusVO));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "关联已有状态")
    @GetMapping(value = "/state_machine/link_status")
    public ResponseEntity<StatusMachineNodeVO> linkStatus(@ApiParam(value = "项目id", required = true)
                                                          @PathVariable("project_id") Long projectId,
                                                          @ApiParam(value = "问题类型id", required = true)
                                                          @RequestParam @Encrypt Long issueTypeId,
                                                          @ApiParam(value = "应用类型")
                                                          @RequestParam(required = false) String applyType,
                                                          @ApiParam(value = "状态id", required = true)
                                                          @RequestParam @Encrypt Long statusId,
                                                          @ApiParam(value = "默认状态", required = true)
                                                          @RequestParam Boolean defaultStatus,
                                                          @ApiParam(value = "是否转换为其他所有")
                                                          @RequestParam(required = false, defaultValue = "true") Boolean transferAll) {
        StatusVO status = new StatusVO();
        status.setId(statusId);
        status.setDefaultStatus(defaultStatus);
        status.setTransferAll(transferAll);
        return Results.success(projectConfigService.linkStatus(projectId, issueTypeId, applyType, status));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除状态机里面的node")
    @DeleteMapping(value = "/state_machine_node/delete")
    public ResponseEntity<Void> deleteNode(@ApiParam(value = "项目id", required = true)
                                           @PathVariable("project_id") Long projectId,
                                           @ApiParam(value = "问题类型id", required = true)
                                           @RequestParam @Encrypt Long issueTypeId,
                                           @ApiParam(value = "应用类型", required = true)
                                           @RequestParam String applyType,
                                           @ApiParam(value = "节点id", required = true)
                                           @RequestParam @Encrypt Long nodeId,
                                           @ApiParam(value = "状态id")
                                           @RequestParam(required = false) @Encrypt Long statusId) {
        projectConfigService.deleteNode(projectId, issueTypeId, applyType, nodeId, statusId);
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "自定义流转列表")
    @GetMapping(value = "/status_transform_setting/list")
    public ResponseEntity<Page<StatusSettingVO>> statusTransformSettingList(@ApiParam(value = "项目id", required = true)
                                                                                @PathVariable("project_id") Long projectId,
                                                                            @SortDefault(value = "smn.rank, smn.id", direction = Sort.Direction.ASC)
                                                                                PageRequest pageRequest,
                                                                            @ApiParam(value = "问题类型id", required = true)
                                                                                @RequestParam @Encrypt Long issueTypeId,
                                                                            @ApiParam(value = "模糊查询")
                                                                                @RequestParam(required = false) String param,
                                                                            @ApiParam(value = "应用类型", required = true)
                                                                                @RequestParam String applyType,
                                                                            @ApiParam(value = "编码", required = true)
                                                                                @RequestParam String schemeCode) {
        return Results.success(projectConfigService.statusTransformSettingList(projectId, issueTypeId, pageRequest, param, applyType, schemeCode));
    }
}
