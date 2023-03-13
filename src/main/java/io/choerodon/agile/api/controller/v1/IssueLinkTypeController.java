package io.choerodon.agile.api.controller.v1;

import java.util.Optional;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import io.choerodon.agile.api.validator.IssueLinkTypeValidator;
import io.choerodon.agile.api.vo.IssueLinkTypeCreateVO;
import io.choerodon.agile.api.vo.IssueLinkTypeSearchVO;
import io.choerodon.agile.api.vo.IssueLinkTypeVO;
import io.choerodon.agile.app.service.IssueLinkTypeService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/6/14
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/issue_link_types")
public class IssueLinkTypeController {

    @Autowired
    private IssueLinkTypeService issueLinkTypeService;
    @Autowired
    private IssueLinkTypeValidator issueLinkTypeValidator;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据项目id查询issueLinkType")
    @PostMapping("/query_all")
    public ResponseEntity<Page<IssueLinkTypeVO>> listIssueLinkType(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable(name = "project_id") Long projectId,
                                                                   @ApiParam(value = "查询信息的项目id")
                                                                   @RequestParam(name = "target_project_id", required = false) Long targetProjectId,
                                                                   @ApiParam(value = "不包含的issueLinkTypeId")
                                                                   @RequestParam(required = false) @Encrypt Long issueLinkTypeId,
                                                                   @ApiParam(value = "查询参数")
                                                                   @RequestBody IssueLinkTypeSearchVO issueLinkTypeSearchVO,
                                                                   @ApiParam(value = "分页信息", required = true)
                                                                   @SortDefault(value = "link_type_id", direction = Sort.Direction.DESC)
                                                                   @ApiIgnore PageRequest pageRequest) {
        return Optional.ofNullable(issueLinkTypeService.listIssueLinkType(projectId, targetProjectId, issueLinkTypeId, issueLinkTypeSearchVO, pageRequest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueLinkType.listIssueLinkType"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据issueLinkTypeId查询issueLinkType")
    @GetMapping(value = "/{linkTypeId}")
    public ResponseEntity<IssueLinkTypeVO> queryIssueLinkType(@ApiParam(value = "项目id", required = true)
                                                              @PathVariable(name = "project_id") Long projectId,
                                                              @ApiParam(value = "linkTypeId", required = true)
                                                              @PathVariable(name = "linkTypeId") @Encrypt Long linkTypeId) {
        return Optional.ofNullable(issueLinkTypeService.queryIssueLinkType(projectId, linkTypeId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueLinkType.queryIssueLinkType"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建issueLinkType")
    @PostMapping
    public ResponseEntity<IssueLinkTypeVO> createIssueLinkType(@ApiParam(value = "项目id", required = true)
                                                               @PathVariable(name = "project_id") Long projectId,
                                                               @ApiParam(value = "创建issueLinkType对象", required = true)
                                                               @RequestBody IssueLinkTypeCreateVO issueLinkTypeCreateVO) {
        issueLinkTypeValidator.verifyCreateData(issueLinkTypeCreateVO, projectId);
        issueLinkTypeValidator.verifyIssueLinkTypeName(projectId, issueLinkTypeCreateVO.getLinkName(), null);
        return Optional.ofNullable(issueLinkTypeService.createIssueLinkType(issueLinkTypeCreateVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.IssueLinkType.createIssueLinkType"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("修改issueLinkType")
    @PutMapping
    public ResponseEntity<IssueLinkTypeVO> updateIssueLinkType(@ApiParam(value = "项目id", required = true)
                                                               @PathVariable(name = "project_id") Long projectId,
                                                               @ApiParam(value = "issueLinkType", required = true)
                                                               @RequestBody IssueLinkTypeVO issueLinkTypeVO) {
        issueLinkTypeValidator.verifyUpdateData(issueLinkTypeVO, projectId);
        issueLinkTypeValidator.verifyIssueLinkTypeName(projectId, issueLinkTypeVO.getLinkName(), issueLinkTypeVO.getLinkTypeId());
        return Optional.ofNullable(issueLinkTypeService.updateIssueLinkType(issueLinkTypeVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.IssueLinkType.updateIssueLinkType"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除issueLink")
    @DeleteMapping(value = "/{issueLinkTypeId}")
    public ResponseEntity deleteIssueLinkType(@ApiParam(value = "项目id", required = true)
                                                  @PathVariable(name = "project_id") Long projectId,
                                              @ApiParam(value = "issueLinkType", required = true)
                                                  @PathVariable(name = "issueLinkTypeId") @Encrypt Long issueLinkTypeId,
                                              @ApiParam(value = "转移到其他的类型上，如果为空则不转移，直接删除")
                                                  @RequestParam(required = false) @Encrypt Long toIssueLinkTypeId) {
        issueLinkTypeValidator.verifyDeleteData(issueLinkTypeId, toIssueLinkTypeId, projectId);
        issueLinkTypeService.deleteIssueLinkType(issueLinkTypeId, toIssueLinkTypeId, projectId);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("IssueLinkType重名校验")
    @GetMapping(value = "/check_name")
    public ResponseEntity checkIssueLinkTypeName(@ApiParam(value = "项目id", required = true)
                                                 @PathVariable(name = "project_id") Long projectId,
                                                 @ApiParam(value = "issue_link_type_name", required = true)
                                                 @RequestParam(name = "issueLinkTypeName") String issueLinkTypeName,
                                                 @ApiParam(value = "issue_link_type_id", required = false)
                                                 @RequestParam(name = "issueLinkTypeId", required = false) @Encrypt Long issueLinkTypeId) {
        return new ResponseEntity<>(issueLinkTypeService.queryIssueLinkTypeName(projectId, issueLinkTypeName, issueLinkTypeId), HttpStatus.OK);
    }
}
