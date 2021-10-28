package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.SprintSearchVO;
import io.choerodon.agile.api.vo.StoryMapStoryVO;
import io.choerodon.agile.api.vo.business.StoryMapDragVO;
import io.choerodon.agile.api.vo.StoryMapVO;
import io.choerodon.agile.app.service.StoryMapService;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.exception.CommonException;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/5/31.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/story_map")
public class StoryMapController {

    @Autowired
    private StoryMapService storyMapService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询故事地图整体")
    @PostMapping("/main")
    public ResponseEntity<StoryMapVO> queryStoryMap(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable(name = "project_id") Long projectId,
                                                    @ApiParam(value = "组织id", required = true)
                                                    @RequestParam Long organizationId,
                                                    @ApiParam(value = "search DTO", required = true)
                                                    @RequestBody SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return Optional.ofNullable(storyMapService.queryStoryMap(projectId, organizationId, searchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.storyMap.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询故事地图需求池")
    @PostMapping("/demand")
    public ResponseEntity<Page<StoryMapStoryVO>> queryStoryMapDemand(@ApiParam(value = "项目id", required = true)
                                                                     @PathVariable(name = "project_id") Long projectId,
                                                                     @ApiParam(value = "search DTO", required = true)
                                                                     @RequestBody SearchVO searchVO,
                                                                     @ApiParam(value = "分页信息", required = true)
                                                                     PageRequest pageRequest) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return Optional.ofNullable(storyMapService.queryStoryMapDemand(projectId, searchVO, pageRequest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.storyMapDemand.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("故事地图移动卡片")
    @PostMapping(value = "/move")
    public ResponseEntity<Void> storyMapMove(@ApiParam(value = "项目id", required = true)
                                       @PathVariable(name = "project_id") Long projectId,
                                       @ApiParam(value = "story map drag DTO", required = true)
                                       @RequestBody @Encrypt StoryMapDragVO storyMapDragVO) {
        storyMapService.storyMapMove(projectId, storyMapDragVO);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("故事地图冲刺泳道查询冲刺的问题计数信息")
    @PostMapping(value = "/sprint_info")
    public ResponseEntity<List<SprintSearchVO>> storyMapSprintInfo(@ApiParam(value = "项目id", required = true)
                                             @PathVariable(name = "project_id") Long projectId,
                                             @RequestBody @Encrypt(ignoreValue = {"0"}) List<Long> sprintIds) {
        return new ResponseEntity<>(storyMapService.storyMapSprintInfo(projectId,sprintIds),HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询故事地图整体(分页)")
    @PostMapping("/page_main")
    public ResponseEntity<StoryMapVO> pageStoryMap(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable(name = "project_id") Long projectId,
                                                    @ApiParam(value = "组织id", required = true)
                                                    @RequestParam Long organizationId,
                                                    @RequestParam Integer page,
                                                    @RequestParam Integer size,
                                                    @ApiParam(value = "search DTO", required = true)
                                                    @RequestBody SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return Optional.ofNullable(storyMapService.pageStoryMap(projectId, organizationId, searchVO, page, size))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.storyMap.get"));
    }
}
