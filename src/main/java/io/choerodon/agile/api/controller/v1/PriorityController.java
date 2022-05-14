package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.validator.PriorityValidator;
import io.choerodon.agile.api.vo.PriorityVO;
import io.choerodon.agile.app.service.PriorityService;

import io.choerodon.agile.infra.utils.ParamUtils;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.exception.CommonException;
import io.choerodon.swagger.annotation.CustomPageRequest;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * @author cong.cheng
 * @date 2018/8/21
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/priority")
public class PriorityController {
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private PriorityValidator priorityValidator;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "展示页面")
    @CustomPageRequest
    @GetMapping
    public ResponseEntity<List<PriorityVO>> selectAll(@ApiParam(value = "组织id", required = true)
                                                      @PathVariable("organization_id") Long organizationId,
                                                      @ApiParam(value = "名称")
                                                      @RequestParam(required = false) String name,
                                                      @ApiParam(value = "描述")
                                                      @RequestParam(required = false) String description,
                                                      @ApiParam(value = "颜色")
                                                      @RequestParam(required = false) String colour,
                                                      @ApiParam(value = "是否为默认")
                                                      @RequestParam(required = false) Boolean isDefault,
                                                      @ApiParam(value = "模糊搜索")
                                                      @RequestParam(required = false) String[] param) {
        PriorityVO priorityVO = new PriorityVO();
        priorityVO.setOrganizationId(organizationId);
        priorityVO.setName(name);
        priorityVO.setDescription(description);
        priorityVO.setColour(colour);
        priorityVO.setDefault(isDefault);
        return new ResponseEntity<>(priorityService.selectAll(priorityVO, ParamUtils.arrToStr(param)), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建优先级")
    @PostMapping
    public ResponseEntity<PriorityVO> create(@ApiParam(value = "组织id", required = true)
                                             @PathVariable("organization_id") Long organizationId,
                                             @ApiParam(value = "优先级对象", required = true)
                                             @RequestBody PriorityVO priorityVO) {
        priorityValidator.createValidate(priorityVO);
        return new ResponseEntity<>(priorityService.create(organizationId, priorityVO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新优先级")
    @PutMapping(value = "/{priority_id}")
    public ResponseEntity<PriorityVO> update(@ApiParam(value = "组织id", required = true)
                                             @PathVariable("organization_id") Long organizationId,
                                             @ApiParam(value = "优先级id", required = true)
                                             @PathVariable("priority_id") @Encrypt Long priorityId,
                                             @ApiParam(value = "优先级对象", required = true)
                                             @RequestBody @Valid PriorityVO priorityVO) {
        priorityVO.setId(priorityId);
        priorityVO.setOrganizationId(organizationId);
        priorityValidator.updateValidate(priorityVO);
        return new ResponseEntity<>(priorityService.update(priorityVO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验优先级名字是否未被使用")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "组织id", required = true)
                                             @PathVariable("organization_id") Long organizationId,
                                             @ApiParam(value = "优先级名称", required = true)
                                             @RequestParam("name") String name) {
        return Optional.ofNullable(priorityService.checkName(organizationId, name))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.priorityName.check"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新展示顺序")
    @PutMapping(value = "/sequence")
    public ResponseEntity<Void> updateByList(@ApiParam(value = "组织id", required = true)
                                             @PathVariable("organization_id") Long organizationId,
                                             @ApiParam(value = "优先级对象集合", required = true)
                                             @RequestBody List<PriorityVO> list) {
        priorityService.updateByList(list, organizationId);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据组织id查询优先级,map")
    @GetMapping("/list")
    public ResponseEntity<Map<Long, PriorityVO>> queryByOrganizationId(@ApiParam(value = "组织id", required = true)
                                                                       @PathVariable("organization_id") Long organizationId) {
        return Optional.ofNullable(priorityService.queryByOrganizationId(organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.priorityList.get"));

    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据组织id查询默认优先级")
    @GetMapping("/default")
    public ResponseEntity<PriorityVO> queryDefaultByOrganizationId(@ApiParam(value = "组织id", required = true)
                                                                   @PathVariable("organization_id") Long organizationId) {
        return Optional.ofNullable(priorityService.queryDefaultByOrganizationId(organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.priority.get"));

    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据组织id查询优先级,list")
    @GetMapping("/list_by_org")
    public ResponseEntity<List<PriorityVO>> queryByOrganizationIdList(@ApiParam(value = "组织id", required = true)
                                                                      @PathVariable("organization_id") Long organizationId) {
        return Optional.ofNullable(priorityService.queryByOrganizationIdList(organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.priorityList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "生效/失效优先级")
    @GetMapping("/enable/{id}")
    public ResponseEntity<PriorityVO> enablePriority(@ApiParam(value = "组织id", required = true)
                                                     @PathVariable("organization_id") Long organizationId,
                                                     @ApiParam(value = "id", required = true)
                                                     @PathVariable @Encrypt Long id,
                                                     @ApiParam(value = "是否为启用")
                                                     @RequestParam(required = false) Boolean enable) {
        return new ResponseEntity<>(priorityService.enablePriority(organizationId, id, enable), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验删除优先级")
    @GetMapping("/check_delete/{id}")
    public ResponseEntity<Long> checkDelete(@ApiParam(value = "组织id", required = true)
                                            @PathVariable("organization_id") Long organizationId,
                                            @ApiParam(value = "id", required = true)
                                            @PathVariable @Encrypt Long id) {
        return new ResponseEntity<>(priorityService.checkDelete(organizationId, id), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除优先级")
    @DeleteMapping("/delete/{id}")
    public ResponseEntity delete(@ApiParam(value = "组织id", required = true)
                                 @PathVariable("organization_id") Long organizationId,
                                 @ApiParam(value = "优先级id", required = true)
                                 @PathVariable("id") @Encrypt Long priorityId,
                                 @ApiParam(value = "修改的目标优先级", required = true)
                                 @RequestParam(required = false) @Encrypt Long changePriorityId) {
        priorityService.delete(organizationId, priorityId, changePriorityId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }
}
