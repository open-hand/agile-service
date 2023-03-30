package io.choerodon.agile.api.controller.v1;

import static io.choerodon.agile.app.service.impl.PersonalFilterServiceImpl.V1;

import java.util.List;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.api.vo.PersonalFilterVO;
import io.choerodon.agile.app.service.PersonalFilterService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author shinan.chen
 * @since 2019/2/25
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/personal_filter")
public class PersonalFilterController {
    @Autowired
    private PersonalFilterService personalFilterService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建我的筛选")
    @PostMapping
    @Deprecated
    public ResponseEntity<PersonalFilterVO> create(@ApiParam(value = "项目id", required = true)
                                                   @PathVariable(name = "project_id") Long projectId,
                                                   @ApiParam(value = "personal filter object", required = true)
                                                   @RequestBody @Encrypt PersonalFilterVO personalFilterVO) {
        return Results.created(personalFilterService.create(0L, projectId, personalFilterVO, V1));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("修改我的筛选")
    @PutMapping(value = "/{filterId}")
    @Deprecated
    public ResponseEntity<PersonalFilterVO> update(@ApiParam(value = "项目id", required = true)
                                                   @PathVariable(name = "project_id") Long projectId,
                                                   @ApiParam(value = "filter id", required = true)
                                                   @PathVariable  @Encrypt Long filterId,
                                                   @ApiParam(value = "personal filter object", required = true)
                                                   @RequestBody  @Encrypt PersonalFilterVO personalFilterVO) {
        return Results.success(personalFilterService.update(0L, projectId, filterId, personalFilterVO, V1));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除我的筛选")
    @DeleteMapping(value = "/{filterId}")
    public ResponseEntity<Void> deleteById(@ApiParam(value = "项目id", required = true)
                                                       @PathVariable(name = "project_id") Long projectId,
                                                       @ApiParam(value = "filter id", required = true)
                                                       @PathVariable  @Encrypt Long filterId) {
        personalFilterService.deleteById(0L, projectId, filterId);
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询我的筛选列表")
    @GetMapping(value = "/query_all/{userId}")
    @Deprecated
    public ResponseEntity<List<PersonalFilterVO>> listByProjectId(@ApiParam(value = "项目id", required = true)
                                                                  @PathVariable(name = "project_id") Long projectId,
                                                                  @ApiParam(value = "用户id", required = true)
                                                                  @PathVariable @Encrypt Long userId,
                                                                  @ApiParam(value = "筛选条件")
                                                                  @RequestParam(name = "searchStr", required = false) String searchStr,
                                                                  @ApiParam(value = "问题类型编码")
                                                                  @RequestParam(name = "filterTypeCode") String filterTypeCode) {
        return Results.success(personalFilterService.listByUserId(0L, projectId, userId, searchStr, filterTypeCode, V1));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id查询我的筛选")
    @GetMapping(value = "/{filterId}")
    public ResponseEntity<PersonalFilterVO> queryById(@ApiParam(value = "项目id", required = true)
                                                      @PathVariable(name = "project_id") Long projectId,
                                                      @ApiParam(value = "filter id", required = true)
                                                      @PathVariable @Encrypt Long filterId) {
        return Results.success(personalFilterService.queryById(0L, projectId, filterId, V1));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("我的筛选重名校验")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(
            @ApiParam(value = "项目id", required = true)  @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "用户id", required = true) @RequestParam @Encrypt Long userId,
            @ApiParam(value = "个人筛选名称", required = true) @RequestParam String name,
            @ApiParam(value = "个人筛选类型", required = true) @RequestParam String filterTypeCode,
            @ApiParam(value = "个人筛选ID(用于更新检查时排除自身)") @RequestParam(required = false) Long filterId
    ) {
        return Results.success(personalFilterService.nameIsExist(0L, projectId, userId, name, filterTypeCode, filterId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("设置默认筛选")
    @PutMapping(value = "/set_default")
    public ResponseEntity<Boolean> setDefault(@ApiParam(value = "项目id", required = true)
                                             @PathVariable(name = "project_id") Long projectId,
                                             @ApiParam(value = "filter Id", required = true)
                                             @RequestParam @Encrypt Long filterId) {
        return Results.success(personalFilterService.setDefault(0L, projectId, filterId));
    }
}
