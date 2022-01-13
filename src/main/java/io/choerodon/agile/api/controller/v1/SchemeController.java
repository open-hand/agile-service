package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.PriorityService;
import io.choerodon.agile.app.service.ProjectConfigService;

import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.agile.infra.utils.ProjectUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.base.BaseController;
import io.choerodon.core.exception.CommonException;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.hzero.starter.keyencrypt.core.IEncryptionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * 根据项目id获取对应数据
 *
 * @author shinan.chen
 * @date 2018/10/24
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
    public ResponseEntity<List<IssueTypeVO>> queryIssueTypesByProjectId(@PathVariable("project_id") Long projectId,
                                                                        @RequestParam("apply_type") String applyType,
                                                                        @RequestParam(value = "only_enabled", defaultValue = "false",
                                                                                required = false) Boolean onlyEnabled) {
        return new ResponseEntity<>(projectConfigService.queryIssueTypesByProjectId(projectId, applyType, onlyEnabled), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目的问题类型列表，带对应的状态机id")
    @GetMapping(value = "/schemes/query_issue_types_with_sm_id")
    public ResponseEntity<List<IssueTypeWithStateMachineIdVO>> queryIssueTypesWithStateMachineIdByProjectId(@PathVariable("project_id") Long projectId,
                                                                                                            @RequestParam(value = "apply_type", required = false) String applyType,
                                                                                                            @RequestParam(value = "only_enabled", defaultValue = "false",
                                                                                                                    required = false) Boolean onlyEnabled) {
        return new ResponseEntity<>(projectConfigService.queryIssueTypesWithStateMachineIdByProjectId(projectId, applyType, onlyEnabled), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下某个问题类型拥有的转换（包含可以转换到的状态）")
    @GetMapping(value = "/schemes/query_transforms")
    public ResponseEntity<List<TransformVO>> queryTransformsByProjectId(@PathVariable("project_id") Long projectId,
                                                                        @RequestParam("current_status_id") @Encrypt Long currentStatusId,
                                                                        @RequestParam("issue_id") @Encrypt Long issueId,
                                                                        @RequestParam("issue_type_id") @Encrypt Long issueTypeId,
                                                                        @RequestParam("apply_type") String applyType) {
        return new ResponseEntity<>(projectConfigService.queryTransformsByProjectId(projectId, currentStatusId, issueId, issueTypeId, applyType), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下所有问题类型所有状态对应的转换")
    @GetMapping(value = "/schemes/query_transforms_map")
    public ResponseEntity<Map<String, Map<String, List>>> queryTransformsMapByProjectId(@PathVariable("project_id") Long projectId,
                                                                                                 @RequestParam(required = false) @Encrypt Long boardId,
                                                                                                 @RequestParam("apply_type") String applyType) {

        return new ResponseEntity<>(EncryptionUtils.encryptMapValueMap(projectConfigService.queryTransformsMapByProjectId(projectId,boardId,applyType)), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下某个问题类型的所有状态")
    @GetMapping(value = "/schemes/query_status_by_issue_type_id")
    public ResponseEntity<List<StatusVO>> queryStatusByIssueTypeId(@PathVariable("project_id") Long projectId,
                                                                   @RequestParam("issue_type_id") @Encrypt Long issueTypeId,
                                                                   @RequestParam("apply_type") String applyType) {
        return new ResponseEntity<>(projectConfigService.queryStatusByIssueTypeId(projectId, issueTypeId, applyType), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下的所有状态")
    @GetMapping(value = "/schemes/query_status_by_project_id")
    public ResponseEntity<List<StatusVO>> queryStatusByProjectId(@PathVariable("project_id") Long projectId,
                                                                 @RequestParam(value = "apply_type", required = false) String applyType) {
        return new ResponseEntity<>(projectConfigService.queryStatusByProjectId(projectId, applyType), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目的问题类型对应的状态机id")
    @GetMapping(value = "/schemes/query_state_machine_id")
    public ResponseEntity<Long> queryStateMachineId(@PathVariable("project_id") Long projectId,
                                                    @RequestParam("apply_type") String applyType,
                                                    @RequestParam("issue_type_id") @Encrypt Long issueTypeId) {
        return new ResponseEntity<>(projectConfigService.queryStateMachineId(projectId, applyType, issueTypeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "【敏捷】新增状态")
    @PostMapping(value = "/schemes/create_status_for_agile")
    public ResponseEntity<StatusVO> createStatusForAgile(@PathVariable("project_id") Long projectId,
                                                         @RequestParam String applyType,
                                                         @RequestBody StatusVO statusVO) {
        return new ResponseEntity<>(projectConfigService.createStatusForAgile(projectId, applyType, statusVO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "【敏捷】校验是否能新增状态")
    @GetMapping(value = "/schemes/check_create_status_for_agile")
    public ResponseEntity<Boolean> checkCreateStatusForAgile(@PathVariable("project_id") Long projectId,
                                                             @RequestParam String applyType) {
        return new ResponseEntity<>((Boolean) projectConfigService.checkCreateStatusForAgile(projectId, applyType).get("flag"), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "【敏捷】校验是否能删除状态")
    @GetMapping(value = "/schemes/check_remove_status_for_agile")
    public ResponseEntity<Boolean> checkRemoveStatusForAgile(@PathVariable("project_id") Long projectId,
                                                             @RequestParam("status_id") @Encrypt Long statusId,
                                                             @RequestParam String applyType) {
        return new ResponseEntity<>(projectConfigService.checkRemoveStatusForAgile(projectId, statusId, applyType), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据项目id查询组织默认优先级")
    @GetMapping("/priority/default")
    public ResponseEntity<PriorityVO> queryDefaultByOrganizationId(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable("project_id") Long projectId) {
        Long organizationId = projectUtil.getOrganizationId(projectId);
        return Optional.ofNullable(priorityService.queryDefaultByOrganizationId(organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.priority.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据项目id查询组织优先级列表")
    @GetMapping("/priority/list_by_org")
    public ResponseEntity<List<PriorityVO>> queryByOrganizationIdList(@ApiParam(value = "项目id", required = true)
                                                                      @PathVariable("project_id") Long projectId) {
        Long organizationId = projectUtil.getOrganizationId(projectId);
        return Optional.ofNullable(priorityService.queryByOrganizationIdList(organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
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
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.firstStatus.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "状态与流转列表")
    @GetMapping(value = "/status_transform/list")
    public ResponseEntity<List<StatusAndTransformVO>> statusTransformList(@PathVariable("project_id") Long projectId,
                                                                          @RequestParam @Encrypt Long issueTypeId,
                                                                          @RequestParam String applyType) {
        return new ResponseEntity<>(projectConfigService.statusTransformList(projectId, issueTypeId, applyType), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "支持对状态机-状态与流转进行排序")
    @PutMapping(value = "/status_transform/sort")
    public ResponseEntity<NodeSortVO> updateSort(@PathVariable("project_id") Long projectId,
                                                 @Encrypt Long statusMachineId,
                                                 @RequestBody NodeSortVO nodeSortVO,
                                                 @RequestParam String applyType) {
        return new ResponseEntity<>(projectConfigService.updateSort(projectId, statusMachineId, nodeSortVO, applyType), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "设置状态机的默认状态")
    @PutMapping(value = "/status_transform/setting_default_status")
    public ResponseEntity settingDefaultStatus(@PathVariable("project_id") Long projectId,
                                               @RequestParam @Encrypt Long issueTypeId,
                                               @RequestParam @Encrypt Long stateMachineId,
                                               @RequestParam @Encrypt Long statusId) {
        projectConfigService.defaultStatus(projectId, issueTypeId, stateMachineId, statusId);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "改变问题类型的转换")
    @PutMapping(value = "/status_transform/update")
    public ResponseEntity<List<StateMachineTransformUpdateVO>> updateTransformByIssueTypeId(@PathVariable("project_id") Long projectId,
                                               @RequestParam @Encrypt Long issueTypeId,
                                               @RequestParam String applyType,
                                               @RequestBody List<StateMachineTransformUpdateVO> list) {

        return new ResponseEntity(projectConfigService.updateTransformByIssueTypeId(projectId, issueTypeId,applyType,list),HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "添加新状态")
    @PostMapping(value = "/status/create")
    public ResponseEntity<StatusVO> createStatus(@PathVariable("project_id") Long projectId,
                                                 @RequestParam(required = false)@Encrypt List<Long> issueTypeIds,
                                                 @RequestParam String applyType,
                                                 @RequestBody StatusVO statusVO) {

        return new ResponseEntity(projectConfigService.createStatus(projectId, issueTypeIds,applyType, statusVO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "关联已有状态")
    @GetMapping(value = "/state_machine/link_status")
    public ResponseEntity<StatusMachineNodeVO> linkStatus(@PathVariable("project_id") Long projectId,
                                                          @RequestParam @Encrypt Long issueTypeId,
                                                          @RequestParam(required = false) String applyType,
                                                          @RequestParam @Encrypt Long statusId,
                                                          @RequestParam Boolean defaultStatus,
                                                          @RequestParam(required = false, defaultValue = "true") Boolean transferAll) {
        StatusVO status = new StatusVO();
        status.setId(statusId);
        status.setDefaultStatus(defaultStatus);
        status.setTransferAll(transferAll);
        return new ResponseEntity(projectConfigService.linkStatus(projectId, issueTypeId, applyType, status), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除状态机里面的node")
    @DeleteMapping(value = "/state_machine_node/delete")
    public ResponseEntity deleteNode(@PathVariable("project_id") Long projectId,
                                                         @RequestParam @Encrypt Long issueTypeId,
                                                         @RequestParam String applyType,
                                                         @RequestParam  @Encrypt Long nodeId,
                                                         @RequestParam(required = false)  @Encrypt Long statusId) {
        projectConfigService.deleteNode(projectId, issueTypeId,applyType,nodeId,statusId);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "自定义流转列表")
    @GetMapping(value = "/status_transform_setting/list")
    public ResponseEntity<Page<StatusSettingVO>> statusTransformSettingList(@PathVariable("project_id") Long projectId,
                                                                            @SortDefault(value = "smn.rank, smn.id", direction = Sort.Direction.ASC)
                                                                            PageRequest pageRequest,
                                                                            @RequestParam @Encrypt Long issueTypeId,
                                                                            @RequestParam(required = false) String param,
                                                                            @RequestParam String applyType,
                                                                            @RequestParam String schemeCode) {
        return new ResponseEntity<>(projectConfigService.statusTransformSettingList(projectId, issueTypeId,pageRequest,param, applyType, schemeCode), HttpStatus.OK);
    }
}
