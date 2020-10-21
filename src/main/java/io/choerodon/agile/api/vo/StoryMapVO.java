package io.choerodon.agile.api.vo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.choerodon.agile.infra.dto.EpicWithInfoDTO;
import io.choerodon.agile.infra.dto.business.StoryMapStoryDTO;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/22 下午3:13
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class StoryMapVO {
    private List<EpicWithInfoDTO> epics;
    private List<StoryMapStoryDTO> storyList;
    private List<StoryMapWidthVO> storyMapWidth;
    private List<StoryMapStoryVO> demandStoryList;

    public List<StoryMapStoryVO> getDemandStoryList() {
        return demandStoryList;
    }

    public void setDemandStoryList(List<StoryMapStoryVO> demandStoryList) {
        this.demandStoryList = demandStoryList;
    }

    public List<EpicWithInfoDTO> getEpics() {
        return epics;
    }

    public void setEpics(List<EpicWithInfoDTO> epics) {
        this.epics = epics;
    }

    public List<StoryMapStoryDTO> getStoryList() {
        return storyList;
    }

    public void setStoryList(List<StoryMapStoryDTO> storyList) {
        this.storyList = storyList;
    }

    public List<StoryMapWidthVO> getStoryMapWidth() {
        return storyMapWidth;
    }

    public void setStoryMapWidth(List<StoryMapWidthVO> storyMapWidth) {
        this.storyMapWidth = storyMapWidth;
    }
}
