package io.choerodon.agile.api.controller.v1;

import java.util.List;
import java.util.Optional;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.api.vo.WikiRelationVO;
import io.choerodon.agile.app.service.WikiRelationService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/12/03.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/knowledge_relation")
public class WikiRelationController {

    @Autowired
    private WikiRelationService wikiRelationService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("添加knowledge relation")
    @PostMapping
    public ResponseEntity<Void> create(@ApiParam(value = "项目id", required = true)
                                 @PathVariable(name = "project_id") Long projectId,
                                 @ApiParam(value = "knowledge relation vo list", required = true)
                                 @RequestBody List<WikiRelationVO> wikiRelationVOList) {
        wikiRelationService.create(projectId, wikiRelationVOList);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据issue id查询knowledge relation")
    @GetMapping("/issue/{issueId}")
    public ResponseEntity<List<WikiRelationVO>> queryByIssueId(@ApiParam(value = "项目id", required = true)
                                                               @PathVariable(name = "project_id") Long projectId,
                                                               @ApiParam(value = "issue id", required = true)
                                                               @PathVariable @Encrypt Long issueId) {
        return Optional.ofNullable(wikiRelationService.queryByIssueId(projectId, issueId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.knowledgeRelationList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id删除knowledge relation")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteById(@ApiParam(value = "项目id", required = true)
                                     @PathVariable(name = "project_id") Long projectId,
                                     @ApiParam(value = "relation id", required = true)
                                     @PathVariable @Encrypt Long id) {
        wikiRelationService.deleteById(projectId, id);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据workSpaceId删除knowledge relation")
    @DeleteMapping("/delete/{space_id}")
    public ResponseEntity<Void> deleteByworkSpaceId(@ApiParam(value = "项目id", required = true)
                                              @PathVariable(name = "project_id") Long projectId,
                                              @ApiParam(value = "workSpaceId", required = true)
                                              @PathVariable(name = "space_id") @Encrypt Long spaceId) {
        wikiRelationService.deleteByWorkSpaceId(projectId, spaceId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }


}