package io.choerodon.agile.api.controller.v1;


import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.core.base.BaseController;
import io.choerodon.core.exception.CommonException;
import io.choerodon.agile.api.vo.IssueTypeSchemeSearchVO;
import io.choerodon.agile.api.vo.IssueTypeSchemeVO;
import io.choerodon.agile.api.vo.IssueTypeSchemeWithInfoVO;
import io.choerodon.agile.app.service.IssueTypeSchemeService;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.swagger.annotation.CustomPageRequest;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import javax.validation.Valid;
import java.util.Map;
import java.util.Optional;

/**
 * @author shinan.chen
 * @date 2018/8/10
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/issue_type_scheme")
public class IssueTypeSchemeController extends BaseController {

    @Autowired
    private IssueTypeSchemeService issueTypeSchemeService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据id查询问题类型方案")
    @GetMapping(value = "/{id}")
    public ResponseEntity<IssueTypeSchemeVO> queryById(@ApiParam(value = "组织id", required = true)
                                                       @PathVariable("organization_id") Long organizationId,
                                                       @ApiParam(value = "问题类型id", required = true)
                                                       @PathVariable("id") @Encrypt Long issueTypeSchemeId) {
        return new ResponseEntity<>(issueTypeSchemeService.queryById(organizationId, 0L, issueTypeSchemeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建问题类型方案")
    @PostMapping
    public ResponseEntity<IssueTypeSchemeVO> create(@ApiParam(value = "组织id", required = true)
                                                    @PathVariable("organization_id") Long organizationId,
                                                    @ApiParam(value = "问题方案", required = true)
                                                    @RequestBody @Valid IssueTypeSchemeVO issueTypeSchemeVO) {
        return new ResponseEntity<>(issueTypeSchemeService.create(organizationId, issueTypeSchemeVO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "修改问题类型方案")
    @PutMapping(value = "/{id}")
    public ResponseEntity<IssueTypeSchemeVO> update(@ApiParam(value = "组织id", required = true)
                                                    @PathVariable("organization_id") Long organizationId,
                                                    @ApiParam(value = "问题方案id", required = true)
                                                    @PathVariable("id") @Encrypt Long issueTypeSchemeId,
                                                    @ApiParam(value = "问题方案", required = true)
                                                    @RequestBody @Valid IssueTypeSchemeVO issueTypeSchemeVO) {
        issueTypeSchemeVO.setId(issueTypeSchemeId);
        issueTypeSchemeVO.setOrganizationId(organizationId);
        return new ResponseEntity<>(issueTypeSchemeService.update(organizationId, issueTypeSchemeVO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验问题类型方案是否可以删除")
    @GetMapping(value = "/check_delete/{id}")
    public ResponseEntity<Map<String, Object>> checkDelete(@ApiParam(value = "组织id", required = true)
                                                           @PathVariable("organization_id") Long organizationId,
                                                           @ApiParam(value = "问题类型方案id", required = true)
                                                           @PathVariable("id") @Encrypt Long issueTypeSchemeId) {
        return new ResponseEntity<>(issueTypeSchemeService.checkDelete(organizationId, issueTypeSchemeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除问题类型方案")
    @DeleteMapping(value = "/{id}")
    public ResponseEntity<Boolean> delete(@ApiParam(value = "组织id", required = true)
                                          @PathVariable("organization_id") Long organizationId,
                                          @ApiParam(value = "问题类型方案id", required = true)
                                          @PathVariable("id") @Encrypt Long issueTypeSchemeId) {
        return new ResponseEntity<>(issueTypeSchemeService.delete(organizationId, issueTypeSchemeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "分页查询问题类型方案列表")
    @CustomPageRequest
    @PostMapping("/list")
    public ResponseEntity<Page<IssueTypeSchemeWithInfoVO>> queryIssueTypeSchemeList(@ApiIgnore
                                                                                    @SortDefault(value = "id", direction = Sort.Direction.DESC) PageRequest pageRequest,
                                                                                    @ApiParam(value = "组织id", required = true)
                                                                                    @PathVariable("organization_id") Long organizationId,
                                                                                    @ApiParam(value = "问题类型方案", required = true)
                                                                                    @RequestBody IssueTypeSchemeSearchVO issueTypeSchemeDTO) {
        return Optional.ofNullable(issueTypeSchemeService.queryIssueTypeSchemeList(pageRequest, organizationId, issueTypeSchemeDTO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.issueTypeSchemeList.get"));

    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验问题类型名字是否未被使用")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "组织id", required = true)
                                             @PathVariable("organization_id") Long organizationId,
                                             @ApiParam(value = "问题类型名称", required = true)
                                             @RequestParam("name") String name,
                                             @ApiParam(value = "问题类型id", required = true)
                                             @RequestParam(value = "id", required = false) @Encrypt Long id) {
        return new ResponseEntity<>(issueTypeSchemeService.checkName(organizationId, name, id), HttpStatus.OK);
    }


}
