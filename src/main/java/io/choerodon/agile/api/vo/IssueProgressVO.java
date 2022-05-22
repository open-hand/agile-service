package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2020-11-25 9:58
 */
public class IssueProgressVO {
    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;
    @ApiModelProperty(value = "未完成数量")
    private Integer unCompletedCount;
    @ApiModelProperty(value = "已完成数量")
    private Integer completedCount;
    @ApiModelProperty(value = "总数")
    private Integer totalCount;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getUnCompletedCount() {
        return unCompletedCount;
    }

    public void setUnCompletedCount(Integer unCompletedCount) {
        this.unCompletedCount = unCompletedCount;
    }

    public Integer getCompletedCount() {
        return completedCount;
    }

    public void setCompletedCount(Integer completedCount) {
        this.completedCount = completedCount;
    }

    public Integer getTotalCount() {
        return totalCount;
    }

    public void setTotalCount(Integer totalCount) {
        this.totalCount = totalCount;
    }
}
