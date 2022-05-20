package io.choerodon.agile.api.vo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.choerodon.agile.infra.dto.EpicWithInfoDTO;
import io.choerodon.agile.infra.dto.business.StoryMapStoryDTO;
import io.swagger.annotations.ApiModelProperty;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/22 下午3:13
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class StoryMapVO {
    @ApiModelProperty(value = "史诗")
    private List<EpicWithInfoDTO> epics;
    @ApiModelProperty(value = "故事")
    private List<StoryMapStoryDTO> storyList;
    @ApiModelProperty(value = "故事地图宽度")
    private List<StoryMapWidthVO> storyMapWidth;
    @ApiModelProperty(value = "故事地图")
    private List<StoryMapStoryVO> demandStoryList;
    @ApiModelProperty(value = "总页数")
    private Integer totalPage;
    @ApiModelProperty(value = "开始页")
    private Integer page;
    @ApiModelProperty(value = "每页数量")
    private Integer size;

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

    public Integer getTotalPage() {
        return totalPage;
    }

    public void setTotalPage(Integer totalPage) {
        this.totalPage = totalPage;
    }

    public Integer getPage() {
        return page;
    }

    public void setPage(Integer page) {
        this.page = page;
    }

    public Integer getSize() {
        return size;
    }

    public void setSize(Integer size) {
        this.size = size;
    }
}
