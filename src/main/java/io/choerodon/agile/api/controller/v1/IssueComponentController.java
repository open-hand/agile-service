package io.choerodon.agile.api.controller.v1;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.ComponentForListVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.SearchVO;

import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.agile.infra.utils.VerifyUpdateUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.exception.CommonException;
import io.choerodon.agile.api.vo.IssueComponentVO;
import io.choerodon.agile.app.service.IssueComponentService;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.CustomPageRequest;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import java.util.List;
import java.util.Optional;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/component")
public class IssueComponentController {

    private static final String SEQUENCE_FIELD_NAME = "sequence";

    @Autowired
    private IssueComponentService issueComponentService;
    @Autowired
    private VerifyUpdateUtil verifyUpdateUtil;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建component")
    @PostMapping
    public ResponseEntity<IssueComponentVO> createComponent(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable(name = "project_id") Long projectId,
                                                            @ApiParam(value = "components对象", required = true)
                                                             @RequestBody IssueComponentVO issueComponentVO) {
        return Optional.ofNullable(issueComponentService.create(projectId, issueComponentVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.component.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("修改component")
    @PutMapping(value = "/{id}")
    public ResponseEntity<IssueComponentVO> updateComponent(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable(name = "project_id") Long projectId,
                                                            @ApiParam(value = "component id", required = true)
                                                             @PathVariable @Encrypt  Long id,
                                                            @ApiParam(value = "components对象", required = true)
                                                             @RequestBody JSONObject jsonObject) {
        IssueComponentVO issueComponentVO = new IssueComponentVO();
        jsonObject.putIfAbsent(SEQUENCE_FIELD_NAME, null);
        List<String> fieldList = verifyUpdateUtil.verifyUpdateData(jsonObject, issueComponentVO);
        return Optional.ofNullable(issueComponentService.update(projectId, id, issueComponentVO, fieldList))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.component.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除component")
    @DeleteMapping(value = "/{id}")
    public ResponseEntity deleteComponent(@ApiParam(value = "项目id", required = true)
                                          @PathVariable(name = "project_id") Long projectId,
                                          @ApiParam(value = "component id", required = true)
                                          @PathVariable @Encrypt Long id,
                                          @ApiParam(value = "relate component id", required = false)
                                          @RequestParam(required = false) @Encrypt Long relateComponentId) {
        issueComponentService.delete(projectId, id, relateComponentId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id查询component")
    @GetMapping(value = "/{id}")
    public ResponseEntity<IssueComponentVO> queryComponentById(@ApiParam(value = "项目id", required = true)
                                                                @PathVariable(name = "project_id") Long projectId,
                                                               @ApiParam(value = "component id", required = true)
                                                                @PathVariable @Encrypt Long id) {
        return Optional.ofNullable(issueComponentService.queryComponentsById(projectId, id))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.component.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据project id查询component")
    @CustomPageRequest
    @PostMapping(value = "/query_all")
    public ResponseEntity<Page<ComponentForListVO>> listByProjectId(@ApiParam(value = "项目id", required = true)
                                                                     @PathVariable(name = "project_id") Long projectId,
                                                                    @ApiParam(value = "当前模块id")
                                                                     @RequestParam(required = false) @Encrypt Long componentId,
                                                                    @ApiParam(value = "是否包含测试")
                                                                     @RequestParam(required = false, name = "no_issue_test", defaultValue = "false") Boolean noIssueTest,
                                                                    @ApiParam(value = "查询参数")
                                                                     @RequestBody(required = false) SearchVO searchVO,
                                                                    @ApiParam(value = "分页信息", required = true)
                                                                     @SortDefault(sort = {"sequence","component_id"}, direction = Sort.Direction.DESC)
                                                                     @ApiIgnore PageRequest pageRequest) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return Optional.ofNullable(issueComponentService.queryComponentByProjectId(projectId, componentId, noIssueTest, searchVO, pageRequest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.componentList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据project id查询component,测试服务用")
    @GetMapping
    public ResponseEntity<List<ComponentForListVO>> listByProjectIdForTest(@ApiParam(value = "项目id", required = true)
                                                                            @PathVariable(name = "project_id") Long projectId,
                                                                           @ApiParam(value = "当前模块id")
                                                                            @RequestParam(required = false) @Encrypt Long componentId,
                                                                           @ApiParam(value = "是否包含测试")
                                                                            @RequestParam(required = false, name = "no_issue_test", defaultValue = "false")
                                                                                    Boolean noIssueTest) {
        return Optional.ofNullable(issueComponentService.listByProjectIdForTest(projectId, componentId, noIssueTest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.componentList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id查询component下的issues")
    @GetMapping(value = "/{id}/issues")
    public ResponseEntity<List<IssueVO>> listByOptions(@ApiParam(value = "项目id", required = true)
                                                        @PathVariable(name = "project_id") Long projectId,
                                                       @ApiParam(value = "component id", required = true)
                                                        @PathVariable @Encrypt Long id) {
        return Optional.ofNullable(issueComponentService.queryIssuesByComponentId(projectId, id))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.issues.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("模块重名校验")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkComponentName(@ApiParam(value = "项目id", required = true)
                                                      @PathVariable(name = "project_id") Long projectId,
                                                      @ApiParam(value = "component name", required = true)
                                                      @RequestParam String componentName) {
        return Optional.ofNullable(issueComponentService.checkComponentName(projectId, componentName))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.checkName.get"));
    }
}
