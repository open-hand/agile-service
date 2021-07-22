package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;

import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.hzero.starter.keyencrypt.core.EncryptContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import springfox.documentation.annotations.ApiIgnore;

import javax.validation.Valid;
import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/4/8
 */
@RestController
@RequestMapping("/v1/projects/{project_id}/field_value")
public class FieldValueController {

    @Autowired
    private PageFieldService pageFieldService;
    @Autowired
    private FieldValueService fieldValueService;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private IssueFieldValueService issueFieldValueService;
    @Autowired
    private FieldOptionService fieldOptionService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "界面上获取字段列表，带有字段选项")
    @PostMapping("/list")
    public ResponseEntity<List<PageFieldViewVO>> queryPageFieldViewList(@ApiParam(value = "项目id", required = true)
                                                                        @PathVariable("project_id") Long projectId,
                                                                        @ApiParam(value = "组织id", required = true)
                                                                        @RequestParam Long organizationId,
                                                                        @ApiParam(value = "参数对象", required = true)
                                                                        @RequestBody @Valid PageFieldViewParamVO paramDTO) {
        return new ResponseEntity<>(pageFieldService.queryPageFieldViewList(organizationId, projectId, paramDTO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据实例id从界面上获取字段列表，带有字段值、字段选项")
    @PostMapping("/list/{instance_id}")
    public ResponseEntity<List<PageFieldViewVO>> queryPageFieldViewListWithInstanceId(@ApiParam(value = "项目id", required = true)
                                                                                      @PathVariable("project_id") Long projectId,
                                                                                      @ApiParam(value = "实例id", required = true)
                                                                                      @PathVariable("instance_id") @Encrypt Long instanceId,
                                                                                      @ApiParam(value = "组织id", required = true)
                                                                                      @RequestParam Long organizationId,
                                                                                      @ApiParam(value = "参数对象", required = true)
                                                                                      @RequestBody @Valid PageFieldViewParamVO paramDTO) {
        return new ResponseEntity<>(pageFieldService.queryPageFieldViewListWithInstanceId(organizationId, projectId, instanceId, paramDTO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建实例时，批量创建字段值")
    @PostMapping("/{instance_id}")
    public ResponseEntity createFieldValues(@ApiParam(value = "项目id", required = true)
                                            @PathVariable("project_id") Long projectId,
                                            @ApiParam(value = "实例id", required = true)
                                            @PathVariable("instance_id") @Encrypt Long instanceId,
                                            @ApiParam(value = "组织id", required = true)
                                            @RequestParam Long organizationId,
                                            @ApiParam(value = "方案编码", required = true)
                                            @RequestParam String schemeCode,
                                            @ApiParam(value = "自定义字段列表（包含值）", required = true)
                                            @RequestBody List<PageFieldViewCreateVO> createDTOs) {
        fieldValueService.createFieldValues(organizationId, projectId, instanceId, schemeCode, createDTOs);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "快速创建实例时，批量创建字段值（默认值）")
    @PostMapping("/quick_create/{instance_id}")
    public ResponseEntity createFieldValuesWithQuickCreate(@ApiParam(value = "项目id", required = true)
                                                           @PathVariable("project_id") Long projectId,
                                                           @ApiParam(value = "实例id", required = true)
                                                           @PathVariable("instance_id") @Encrypt Long instanceId,
                                                           @ApiParam(value = "组织id", required = true)
                                                           @RequestParam Long organizationId,
                                                           @ApiParam(value = "参数对象", required = true)
                                                           @RequestBody @Valid PageFieldViewParamVO paramDTO) {
        fieldValueService.createFieldValuesWithQuickCreate(organizationId, projectId, instanceId, paramDTO);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "保存值/修改值")
    @PostMapping("/update/{instance_id}")
    public ResponseEntity<List<FieldValueVO>> updateFieldValue(@ApiParam(value = "项目id", required = true)
                                                               @PathVariable("project_id") Long projectId,
                                                               @ApiParam(value = "组织id", required = true)
                                                               @RequestParam Long organizationId,
                                                               @ApiParam(value = "实例id", required = true)
                                                               @PathVariable("instance_id") @Encrypt Long instanceId,
                                                               @ApiParam(value = "字段id", required = true)
                                                               @RequestParam @Encrypt Long fieldId,
                                                               @ApiParam(value = "方案编码", required = true)
                                                               @RequestParam String schemeCode,
                                                               @ApiParam(value = "字段编码")
                                                               @RequestParam(required = false) String fieldCode,
                                                               @ApiParam(value = "值对象列表", required = true)
                                                               @RequestBody PageFieldViewUpdateVO updateDTO) {
        return new ResponseEntity<>(fieldValueService.updateFieldValue(organizationId, projectId, instanceId, fieldId, schemeCode, updateDTO, fieldCode), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "【敏捷专用】问题管理界面获取自定义字段表头")
    @GetMapping("/list/getFields")
    public ResponseEntity<List<AgileIssueHeadVO>> getIssueHeadForAgile(@ApiParam(value = "项目id", required = true)
                                                                       @PathVariable("project_id") Long projectId,
                                                                       @ApiParam(value = "组织id", required = true)
                                                                       @RequestParam Long organizationId,
                                                                       @ApiParam(value = "方案编码", required = true)
                                                                       @RequestParam String schemeCode,
                                                                       @ApiParam(value = "字段类型", required = true)
                                                                       @RequestParam(defaultValue = "null") String issueTypeList) {
        return new ResponseEntity<>(objectSchemeFieldService.getIssueHeadForAgile(organizationId, projectId, schemeCode, issueTypeList), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取项目下的自定义字段")
    @GetMapping("/list/custom_field")
    public ResponseEntity<List<ObjectSchemeFieldDetailVO>> queryCustomFieldList(@ApiParam(value = "项目id", required = true)
                                                                                @PathVariable("project_id") Long projectId,
                                                                                @RequestParam(defaultValue = "null") String issueTypeList) {
        return new ResponseEntity<>(objectSchemeFieldService.queryCustomFieldListWithOutOption(projectId, issueTypeList), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取项目下的所有字段（系统字段和自定义字段）")
    @GetMapping("/list/fields")
    public ResponseEntity<List<ObjectSchemeFieldDetailVO>> listFieldsWithOptionals(@ApiParam(value = "项目id", required = true)
                                                                                   @PathVariable("project_id") Long projectId,
                                                                                   @RequestParam @Encrypt Long issueTypeId,
                                                                                   @RequestParam Long organizationId) {
        return new ResponseEntity<>(objectSchemeFieldService.listFieldsWithOptionals(projectId, issueTypeId, organizationId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "批量修改预定义字段值和自定义字段值")
    @PostMapping("/batch_update_fields_value")
    public ResponseEntity batchUpdateFieldsValue(@ApiParam(value = "项目id", required = true)
                                                   @PathVariable("project_id") Long projectId,
                                                   @ApiParam(value = "方案编码", required = true)
                                                   @RequestParam String schemeCode,
                                                   @RequestParam String applyType,
                                                   @RequestBody @Encrypt BatchUpdateFieldsValueVo batchUpdateFieldsValueVo) {
        issueFieldValueService.asyncUpdateFields(projectId,schemeCode,batchUpdateFieldsValueVo,applyType, (ServletRequestAttributes)RequestContextHolder.currentRequestAttributes(), EncryptContext.encryptType().name());
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "快速创建根据问题类型获取概要字段默认值")
    @GetMapping("/summary_default_value")
    public ResponseEntity<String> getIssueSummaryDefaultValue(@ApiParam(value = "项目id", required = true)
                                                              @PathVariable("project_id") Long projectId,
                                                              @ApiParam(value = "组织id", required = true)
                                                              @RequestParam Long organizationId,
                                                              @RequestParam @Encrypt Long issueTypeId) {
        return new ResponseEntity<>(objectSchemeFieldService.getIssueSummaryDefaultValue(organizationId, projectId, issueTypeId), HttpStatus.OK);
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "界面获取字段表头(包含系统字段和自定义字段)")
    @GetMapping("/list/get_fields")
    public ResponseEntity<List<ObjectSchemeFieldVO>> getFields(@ApiParam(value = "项目id", required = true)
                                                                       @PathVariable("project_id") Long projectId,
                                                                       @ApiParam(value = "组织id", required = true)
                                                                       @RequestParam Long organizationId,
                                                                       @ApiParam(value = "方案编码", required = true)
                                                                       @RequestParam String schemeCode,
                                                                       @ApiParam(value = "字段类型", required = true)
                                                                       @RequestParam String issueTypeList) {
        return new ResponseEntity<>(objectSchemeFieldService.getAllField(organizationId, projectId, schemeCode, issueTypeList), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "分页获取自定义字段下的选项")
    @GetMapping("/{field_id}/options")
    public ResponseEntity<Page<FieldOptionVO>> getOptionsPageByFieldId(
            @ApiParam(value = "项目id", required = true)
            @PathVariable("project_id") Long projectId,
            @ApiParam(value = "组织id", required = true)
            @RequestParam Long organizationId,
            @PathVariable("field_id") @Encrypt Long fieldId,
            @ApiParam(value = "搜索参数，支持模糊查询", required = true)
            @RequestParam(required = false) String searchValue,
            @ApiParam(value = "已选择的选项id", required = true)
            @Encrypt @RequestParam(required = false) List<Long> selected,
            @RequestParam(required = false, defaultValue = "false") Boolean enabled,
            @ApiParam(value = "分页信息", required = true)
            @ApiIgnore PageRequest pageRequest) {
        return new ResponseEntity<>(fieldOptionService.getOptionsPageByFieldId(organizationId, fieldId, searchValue, selected, enabled, pageRequest), HttpStatus.OK);
    }
}
