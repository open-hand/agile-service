package io.choerodon.agile.api.controller.v1;


import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.ProjectIssueTypeVO;
import io.choerodon.agile.infra.utils.VerifyUpdateUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.core.base.BaseController;
import io.choerodon.agile.api.vo.IssueTypeSearchVO;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.app.service.IssueTypeService;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import javax.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * @author shinan.chen
 * @date 2018/8/8
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/issue_type")
public class IssueTypeController extends BaseController {

    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private VerifyUpdateUtil verifyUpdateUtil;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织层分页查询问题类型列表")
    @PostMapping("/list")
    public ResponseEntity<Page<IssueTypeVO>> queryIssueTypeList(@ApiIgnore
                                                                @SortDefault(value = "id", direction = Sort.Direction.DESC) PageRequest pageRequest,
                                                                @ApiParam(value = "组织id", required = true)
                                                                @PathVariable("organization_id") Long organizationId,
                                                                @ApiParam(value = "issueTypeSearch", required = true)
                                                                @RequestBody IssueTypeSearchVO issueTypeSearchVO) {
        return ResponseEntity.ok(issueTypeService.pagedQuery(pageRequest, organizationId, 0L, issueTypeSearchVO));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织层创建问题类型")
    @PostMapping
    public ResponseEntity<IssueTypeVO> create(@PathVariable("organization_id") Long organizationId,
                                              @RequestBody @Valid IssueTypeVO issueTypeVO) {
        issueTypeVO.setSource(null);
        issueTypeVO.setReferenceId(null);
        return ResponseEntity.ok(issueTypeService.create(organizationId, 0L, issueTypeVO));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织层查询问题类型详情")
    @GetMapping("/{id}")
    public ResponseEntity<IssueTypeVO> query(@ApiParam(value = "组织id", required = true)
                                             @PathVariable("organization_id") Long organizationId,
                                             @PathVariable("id") @Encrypt Long issueTypeId) {
        return ResponseEntity.ok(issueTypeService.query(organizationId, 0L, issueTypeId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织层修改问题类型")
    @PutMapping(value = "/{id}")
    public ResponseEntity<IssueTypeVO> update(@PathVariable("organization_id") Long organizationId,
                                              @PathVariable("id") @Encrypt Long issueTypeId,
                                              @RequestBody JSONObject jsonObject) {
        IssueTypeVO issueTypeVO = new IssueTypeVO();
        List<String> fieldList = verifyUpdateUtil.verifyUpdateData(jsonObject, issueTypeVO);
        issueTypeVO.setId(issueTypeId);
        issueTypeVO.setOrganizationId(organizationId);
        issueTypeVO.setProjectId(0L);
        return new ResponseEntity<>(issueTypeService.update(issueTypeVO, fieldList), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验问题类型名字是否未被使用")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(@PathVariable("organization_id") Long organizationId,
                                             @RequestParam("name") String name,
                                             @RequestParam(value = "id", required = false) @Encrypt Long id) {
        return new ResponseEntity<>(issueTypeService.checkName(organizationId, 0L, name, id), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除问题类型")
    @DeleteMapping(value = "/{id}")
    public ResponseEntity delete(@PathVariable("organization_id") Long organizationId,
                                 @PathVariable("id") @Encrypt Long issueTypeId) {
        issueTypeService.delete(organizationId, 0L, issueTypeId);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新组织问题类型是否可以被引用")
    @PutMapping(value = "/{id}/update_referenced")
    public ResponseEntity updateReferenced(@PathVariable("organization_id") Long organizationId,
                                           @PathVariable(value = "id") @Encrypt Long issueTypeId,
                                           @RequestParam Boolean referenced) {
        issueTypeService.updateReferenced(organizationId, issueTypeId, referenced);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织问题类型查询使用详情")
    @GetMapping(value = "/{id}/usage_detail")
    public ResponseEntity<Page<ProjectIssueTypeVO>> usageDetail(@PathVariable("organization_id") Long organizationId,
                                                                @PathVariable(value = "id") @Encrypt Long issueTypeId,
                                                                PageRequest pageRequest) {
        return ResponseEntity.ok(issueTypeService.usageDetail(organizationId, issueTypeId, pageRequest));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "迁移组织层问题类型数据")
    @PostMapping(value = "/init_data")
    public ResponseEntity<Map<Long, Map<String, Long>>> initIssueTypeData(@PathVariable("organization_id") Long organizationId,
                                                                          @RequestBody List<Long> orgIds) {
        return new ResponseEntity<>(issueTypeService.initIssueTypeData(organizationId, orgIds), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验问题类型名字是否未被使用")
    @GetMapping(value = "/check_icon")
    public ResponseEntity<Boolean> checkIcon(@PathVariable("organization_id") Long organizationId,
                                             @RequestParam("icon") String icon,
                                             @RequestParam(value = "id", required = false) @Encrypt Long id) {
        return new ResponseEntity<>(issueTypeService.checkIcon(organizationId, 0L, icon, id), HttpStatus.OK);
    }
}
