package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.SprintSearchVO;
import io.choerodon.agile.api.vo.StoryMapStoryVO;
import io.choerodon.agile.api.vo.business.StoryMapDragVO;
import io.choerodon.agile.api.vo.StoryMapVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/5/31.
 * Email: fuqianghuang01@gmail.com
 */
public interface StoryMapService {

    StoryMapVO queryStoryMap(Long projectId, Long organizationId, SearchVO searchVO);

    Page<StoryMapStoryVO> queryStoryMapDemand(Long projectId, SearchVO searchVO, PageRequest pageRequest);

    void storyMapMove(Long projectId, StoryMapDragVO storyMapDragVO);

    List<SprintSearchVO> storyMapSprintInfo(Long projectId, List<Long> sprintIds);

    StoryMapVO pageStoryMap(Long projectId, Long organizationId, SearchVO searchVO,Integer page, Integer size);
}
