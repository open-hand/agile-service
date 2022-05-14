package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.StatusTransferSettingDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2021-03-22 14:39
 */
@RestController
@RequestMapping("/v1/organizations/{organization_id}/organization_config")
public class OrganizationConfigController {

    @Autowired
    private OrganizationConfigService organizationConfigService;
    @Autowired
    private StatusNoticeSettingService statusNoticeSettingService;
    @Autowired
    private StatusTransferSettingService statusTransferSettingService;
    @Autowired
    private StatusLinkageService statusLinkageService;
    @Autowired
    private StatusFieldSettingService statusFieldSettingService;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织层创建问题类型的状态机模板")
    @GetMapping("/init_status_machine_template")
    public ResponseEntity initStateMachineTemplate(@ApiParam(value = "组织id", required = true)
                                                   @PathVariable("organization_id") Long organizationId,
                                                   @ApiParam(value = "问题类型Id", required = true)
                                                   @RequestParam @Encrypt Long issueTypeId) {
        organizationConfigService.initStatusMachineTemplate(organizationId, issueTypeId);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织层状态机模板的状态转换")
    @GetMapping("/list_transform")
    public ResponseEntity listTransform(@ApiParam(value = "组织id", required = true)
                                        @PathVariable("organization_id") Long organizationId,
                                        @ApiParam(value = "问题类型Id", required = true)
                                        @RequestParam @Encrypt Long issueTypeId) {
        return Optional.ofNullable(organizationConfigService.listTransform(organizationId, issueTypeId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.status.machine.transform.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织层状态机模板配置状态转换")
    @PutMapping("/status_machine_transform/update")
    public ResponseEntity<List<StateMachineTransformUpdateVO>> updateTransformByIssueTypeId(@ApiParam(value = "组织id", required = true)
                                                                                            @PathVariable("organization_id") Long organizationId,
                                                                                            @ApiParam(value = "问题类型Id", required = true)
                                                                                            @RequestParam @Encrypt Long issueTypeId,
                                                                                            @ApiParam(value = "状态转换更新对象", required = true)
                                                                                            @RequestBody List<StateMachineTransformUpdateVO> list) {
        return Optional.ofNullable(organizationConfigService.updateTransform(organizationId, issueTypeId, list))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.status.machine.transform.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织层状态机模板删除节点")
    @DeleteMapping("/status_machine_node/delete")
    public ResponseEntity deleteStatusMachineNode(@ApiParam(value = "组织id", required = true)
                                                  @PathVariable("organization_id") Long organizationId,
                                                  @ApiParam(value = "问题类型Id", required = true)
                                                  @RequestParam @Encrypt Long issueTypeId,
                                                  @ApiParam(value = "状态机节点Id", required = true)
                                                  @RequestParam @Encrypt Long nodeId) {
        organizationConfigService.deleteNode(organizationId, issueTypeId, nodeId);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织层状态机创建状态")
    @PostMapping("/status/create")
    public ResponseEntity<StatusVO> createStatus(@ApiParam(value = "组织id", required = true)
                                                 @PathVariable("organization_id") Long organizationId,
                                                 @ApiParam(value = "问题类型Id", required = true)
                                                 @RequestParam @Encrypt List<Long> issueTypeId,
                                                 @ApiParam(value = "状态对象", required = true)
                                                 @RequestBody StatusVO statusVO) {
        return Optional.ofNullable(organizationConfigService.createStatus(organizationId, issueTypeId, statusVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.status.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织层状态机创建状态")
    @PostMapping("/status/link_status")
    public ResponseEntity<StatusMachineNodeVO> linkStatus(@ApiParam(value = "组织id", required = true)
                                                          @PathVariable("organization_id") Long organizationId,
                                                          @ApiParam(value = "问题类型Id", required = true)
                                                          @RequestParam @Encrypt Long issueTypeId,
                                                          @ApiParam(value = "状态id", required = true)
                                                          @RequestParam @Encrypt Long statusId,
                                                          @ApiParam(value = "是否为默认状态", required = true)
                                                          @RequestParam Boolean defaultStatus,
                                                          @ApiParam(value = "是否转换为其他全部状态", required = true)
                                                          @RequestParam Boolean transferAll) {
        return Optional.ofNullable(organizationConfigService.linkStatus(organizationId, issueTypeId, statusId, defaultStatus, transferAll))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.link.status"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "设置默认状态")
    @PostMapping("/status_machine_node/default_status")
    public ResponseEntity<StatusVO> defaultStatus(@ApiParam(value = "组织id", required = true)
                                                  @PathVariable("organization_id") Long organizationId,
                                                  @ApiParam(value = "状态机Id", required = true)
                                                  @RequestParam @Encrypt Long stateMachineId,
                                                  @ApiParam(value = "状态Id", required = true)
                                                  @RequestParam @Encrypt Long statusId) {
        organizationConfigService.defaultStatus(organizationId, stateMachineId, statusId);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @ApiOperation(value = "组织层状态机模板查询消息通知明细")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/status_notice_settings/detail")
    public ResponseEntity<StatusNoticeSettingVO> detail(@PathVariable("organization_id") Long organizationId,
                                                        @ApiParam(value = "问题类型Id", required = true)
                                                        @RequestParam("issue_type_id") @Encrypt Long issueTypeId,
                                                        @ApiParam(value = "状态Id", required = true)
                                                        @RequestParam("status_id") @Encrypt Long statusId,
                                                        @ApiParam(value = "方案编码", required = true)
                                                        @RequestParam(required = false) String schemeCode) {
        return Results.success(statusNoticeSettingService.statusNoticeDetail(organizationId, issueTypeId, statusId, schemeCode));
    }

    @ApiOperation(value = "组织层状态机模板保存消息通知")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PostMapping("/status_notice_settings/save")
    public ResponseEntity<Void> saveStatusNotice(@ApiParam(value = "组织id", required = true)
                                                 @PathVariable("organization_id") Long organizationId,
                                                 @ApiParam(value = "状态消息通知配置", required = true)
                                                 @RequestBody StatusNoticeSettingVO statusNoticeSettingVO) {
        statusNoticeSettingService.saveStatusNotice(organizationId, statusNoticeSettingVO);
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "自定义流转列表")
    @GetMapping(value = "/status_transform_setting/list")
    public ResponseEntity<Page<StatusSettingVO>> statusTransformSettingList(@PathVariable("organization_id") Long organizationId,
                                                                            @SortDefault(value = "smn.rank, smn.id", direction = Sort.Direction.ASC)
                                                                            PageRequest pageRequest,
                                                                            @ApiParam(value = "问题类型Id", required = true)
                                                                            @RequestParam @Encrypt Long issueTypeId,
                                                                            @ApiParam(value = "过滤参数:状态名称", required = true)
                                                                            @RequestParam(required = false) String param,
                                                                            @ApiParam(value = "方案code", required = true)
                                                                            @RequestParam String schemeCode) {
        return Optional.ofNullable(organizationConfigService.statusTransformSettingList(organizationId, issueTypeId, pageRequest, param, schemeCode))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.status.transform.setting.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "为问题类型的状态创建流转条件")
    @PostMapping("/status_transfer_setting/create")
    public ResponseEntity createOrUpdate(@ApiParam(value = "组织id", required = true)
                                         @PathVariable("organization_id") Long organizationId,
                                         @ApiParam(value = "问题类型id", required = true)
                                         @RequestParam @Encrypt Long issueTypeId,
                                         @ApiParam(value = "状态id", required = true)
                                         @RequestParam @Encrypt Long statusId,
                                         @ApiParam(value = "乐观锁", required = true)
                                         @RequestParam Long objectVersionNumber,
                                         @ApiParam(value = "流转条件", required = true)
                                         @RequestBody List<StatusTransferSettingCreateVO> list) {
        statusTransferSettingService.saveStatusTransfer(organizationId, issueTypeId, statusId, objectVersionNumber, list);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询流转条件")
    @GetMapping(value = "/status_transfer_setting/query_transfer")
    public ResponseEntity<List<StatusTransferSettingDTO>> queryStatusTransferSetting(@ApiParam(value = "组织id", required = true)
                                                                                     @PathVariable("organization_id") Long organizationId,
                                                                                     @ApiParam(value = "问题类型id", required = true)
                                                                                     @RequestParam @Encrypt Long issueTypeId,
                                                                                     @ApiParam(value = "状态id", required = true)
                                                                                     @RequestParam @Encrypt Long statusId) {
        return Optional.ofNullable(statusTransferSettingService.listByOptions(organizationId, issueTypeId, statusId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.status.transfer.setting.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("为状态配置父子级联动")
    @PostMapping("/status_linkages/save")
    public ResponseEntity<List<StatusLinkageVO>> saveStatusLinkage(@ApiParam(value = "组织Id", required = true)
                                                                   @PathVariable(name = "organization_id") Long organizationId,
                                                                   @ApiParam(value = "问题类型Id", required = true)
                                                                   @RequestParam @Encrypt Long issueTypeId,
                                                                   @ApiParam(value = "状态Id", required = true)
                                                                   @RequestParam @Encrypt Long statusId,
                                                                   @ApiParam(value = "node的乐观锁", required = true)
                                                                   @RequestParam Long objectVersionNumber,
                                                                   @ApiParam(value = "父子联动集合", required = true)
                                                                   @RequestBody List<StatusLinkageVO> list) {
        return Optional.ofNullable(statusLinkageService.saveStatusLinkage(organizationId, issueTypeId, statusId, objectVersionNumber, list))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.status.linkage.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("组织层状态机查询状态设置的联动")
    @GetMapping("/status_linkages/list")
    public ResponseEntity<List<StatusLinkageVO>> listStatusLinkage(@ApiParam(value = "组织Id", required = true)
                                                                   @PathVariable(name = "organization_id") Long organizationId,
                                                                   @ApiParam(value = "问题类型Id", required = true)
                                                                   @RequestParam @Encrypt Long issueTypeId,
                                                                   @ApiParam(value = "状态Id", required = true)
                                                                   @RequestParam @Encrypt Long statusId) {
        return Optional.ofNullable(statusLinkageService.listByOptions(organizationId, issueTypeId, statusId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.status.linkage.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("组织层为问题类型配置状态流转后的更改属性")
    @PostMapping("/status_field_settings/save")
    public ResponseEntity<List<StatusFieldSettingVO>> saveStatusFieldSettings(@ApiParam(value = "组织Id", required = true)
                                                                              @PathVariable(name = "organization_id") Long organizationId,
                                                                              @ApiParam(value = "问题类型Id", required = true)
                                                                              @RequestParam @Encrypt Long issueTypeId,
                                                                              @ApiParam(value = "状态Id", required = true)
                                                                              @RequestParam @Encrypt Long statusId,
                                                                              @ApiParam(value = "node的乐观锁", required = true)
                                                                              @RequestParam Long objectVersionNumber,
                                                                              @ApiParam(value = "状态流转更改属性配置", required = true)
                                                                              @RequestBody List<StatusFieldSettingVO> list) {
        return Optional.ofNullable(statusFieldSettingService.saveStatusFieldSettings(organizationId, issueTypeId, statusId, objectVersionNumber, list))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.status.field.setting.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("组织层查询状态设置的更新属性")
    @GetMapping("/status_field_settings/list")
    public ResponseEntity<List<StatusFieldSettingVO>> listFieldSetting(@ApiParam(value = "组织Id", required = true)
                                                                       @PathVariable(name = "organization_id") Long organizationId,
                                                                       @ApiParam(value = "问题类型Id", required = true)
                                                                       @RequestParam @Encrypt Long issueTypeId,
                                                                       @ApiParam(value = "状态Id", required = true)
                                                                       @RequestParam @Encrypt Long statusId) {
        return Optional.ofNullable(statusFieldSettingService.listByOptions(organizationId, issueTypeId, statusId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.status.field.setting.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据方案编码获取人员自定义字段")
    @GetMapping("/member_list")
    public ResponseEntity<List<ObjectSchemeFieldVO>> selectMemberList(@ApiParam(value = "组织id", required = true)
                                                                      @PathVariable("organization_id") Long organizationId,
                                                                      @ApiParam(value = "issue类型id", required = true)
                                                                      @RequestParam @Encrypt Long issueTypeId,
                                                                      @ApiParam(value = "方案编码")
                                                                      @RequestParam(required = false) String schemeCode) {
        return new ResponseEntity<>(objectSchemeFieldService.selectMemberList(organizationId, null, schemeCode, issueTypeId, null), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验组织是否维护模板")
    @GetMapping("/check_config_template")
    public ResponseEntity<Map<String,Boolean>> checkConfigTemplate(@ApiParam(value = "组织id", required = true)
                                                       @PathVariable("organization_id") Long organizationId) {
        return new ResponseEntity<>(organizationConfigService.checkConfigTemplate(organizationId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验组织层问题类型是否设置状态机模板")
    @GetMapping("/check_status_machine_template")
    public ResponseEntity<Boolean> checkStatusMachineTemplate(@ApiParam(value = "组织id", required = true)
                                                                           @PathVariable("organization_id") Long organizationId,
                                                                           @ApiParam(value = "issue类型id", required = true)
                                                                           @RequestParam @Encrypt Long issueTypeId) {
        return new ResponseEntity<>(organizationConfigService.checkStatusMachineTemplate(organizationId, issueTypeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询组织层配置状态机模板的问题类型")
    @GetMapping("/issue_type/list")
    public ResponseEntity<List<IssueTypeVO>> listIssueType(@ApiParam(value = "组织id", required = true)
                                                              @PathVariable("organization_id") Long organizationId) {
        return new ResponseEntity<>(organizationConfigService.listIssueType(organizationId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "修改项目类型判断项目是否已经初始化过状态机")
    @GetMapping("/check_configured")
    public ResponseEntity<Boolean> checkConfigured(@ApiParam(value = "组织id", required = true)
                                               @PathVariable("organization_id") Long organizationId,
                                               @ApiParam(value = "项目Id", required = true)
                                               @RequestParam Long projectId) {
        return new ResponseEntity<>(organizationConfigService.checkConfigured(organizationId,projectId), HttpStatus.OK);
    }


    @ApiOperation(value = "分支合并状态流转配置")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/status_branch_merge_setting/query")
    public ResponseEntity<StatusBranchMergeSettingVO> queryStatusBranchMergeSetting(@ApiParam(value = "组织id", required = true)
                                                                                    @PathVariable("organization_id") Long organizationId,
                                                                                    @ApiParam(value = "问题类型id", required = true)
                                                                                    @RequestParam @Encrypt Long issueTypeId,
                                                                                    @ApiParam(value = "状态id", required = true)
                                                                                    @RequestParam @Encrypt Long statusId) {
        return Results.success(organizationConfigService.queryStatusBranchMergeSetting(organizationId, issueTypeId, statusId));
    }

    @ApiOperation(value = "分支合并状态流转配置")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PutMapping("/status_branch_merge_setting/update")
    public ResponseEntity updateAutoTransform(@ApiParam(value = "组织", required = true)
                                              @PathVariable("organization_id") Long organizationId,
                                              @ApiParam(value = "问题类型id", required = true)
                                              @RequestParam @Encrypt Long issueTypeId,
                                              @ApiParam(value = "状态id", required = true)
                                              @RequestParam @Encrypt Long statusId,
                                              @ApiParam(value = "是否允许自动流转", required = true)
                                              @RequestParam Boolean autoTransform) {
        organizationConfigService.updateAutoTransform(organizationId, issueTypeId, statusId, autoTransform);
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "支持对状态机-状态与流转进行排序")
    @PutMapping(value = "/status_transform/sort")
    public ResponseEntity<NodeSortVO> updateSort(@ApiParam(value = "组织id", required = true)
                                                 @PathVariable("organization_id") Long organizationId,
                                                 @ApiParam(value = "状态机id", required = true)
                                                 @Encrypt Long statusMachineId,
                                                 @ApiParam(value = "节点", required = true)
                                                 @RequestBody NodeSortVO nodeSortVO) {
        return new ResponseEntity<>(organizationConfigService.updateSort(organizationId, statusMachineId, nodeSortVO), HttpStatus.OK);
    }

}
