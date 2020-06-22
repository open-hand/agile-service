package io.choerodon.agile.app.service;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.StoryMapDragVO;
import io.choerodon.agile.api.vo.StoryMapVO;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/5/31.
 * Email: fuqianghuang01@gmail.com
 */
public interface StoryMapService {

    StoryMapVO queryStoryMap(Long projectId, Long organizationId, SearchVO searchVO);

    StoryMapVO queryStoryMapDemand(Long projectId, SearchVO searchVO);

    void storyMapMove(Long projectId, StoryMapDragVO storyMapDragVO);

}
