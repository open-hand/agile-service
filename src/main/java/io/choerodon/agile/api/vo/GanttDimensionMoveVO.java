package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * @author superlee
 * @since 2021-10-09
 */
public class GanttDimensionMoveVO {

    @NotEmpty(message = "error.gantt.dimension.null")
    @ApiModelProperty(value = "纬度")
    private String dimension;
    @Encrypt
    @ApiModelProperty(value = "前一个id")
    private Long previousId;
    @Encrypt
    @ApiModelProperty(value = "后一个id")
    private Long nextId;
    @Encrypt
    @NotNull(message = "error.gantt.currentId.null")
    @ApiModelProperty(value = "当前id")
    private Long currentId;
    @NotNull(message = "error.gantt.searchVO.null")
    @ApiModelProperty(value = "筛选条件")
    private SearchVO searchVO;

    public String getDimension() {
        return dimension;
    }

    public void setDimension(String dimension) {
        this.dimension = dimension;
    }

    public Long getPreviousId() {
        return previousId;
    }

    public void setPreviousId(Long previousId) {
        this.previousId = previousId;
    }

    public Long getNextId() {
        return nextId;
    }

    public void setNextId(Long nextId) {
        this.nextId = nextId;
    }

    public Long getCurrentId() {
        return currentId;
    }

    public void setCurrentId(Long currentId) {
        this.currentId = currentId;
    }

    public SearchVO getSearchVO() {
        return searchVO;
    }

    public void setSearchVO(SearchVO searchVO) {
        this.searchVO = searchVO;
    }
}
