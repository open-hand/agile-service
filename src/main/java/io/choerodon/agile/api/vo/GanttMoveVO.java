package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * @author superlee
 * @since 2021-09-28
 */
public class GanttMoveVO {
    @NotEmpty(message = "error.gantt.dimension.null")
    private String dimension;
    @Encrypt
    private Long previousId;
    @Encrypt
    private Long nextId;
    @Encrypt
    @NotNull(message = "error.gantt.currentId.null")
    private Long currentId;
    @Encrypt(ignoreValue = {"0"})
    @NotNull(message = "error.gantt.instanceId.null")
    private Long instanceId;
    @NotEmpty(message = "error.gantt.instanceType.null")
    private String instanceType;
    @NotNull(message = "error.gantt.searchVO.null")
    private SearchVO searchVO;

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

    public Long getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(Long instanceId) {
        this.instanceId = instanceId;
    }

    public String getInstanceType() {
        return instanceType;
    }

    public void setInstanceType(String instanceType) {
        this.instanceType = instanceType;
    }

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
}
