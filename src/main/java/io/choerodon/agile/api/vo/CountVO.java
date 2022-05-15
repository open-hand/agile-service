package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.apache.ibatis.annotations.Param;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2021-10-11 15:29
 */
public class CountVO {
    @Encrypt
    @ApiModelProperty("id")
    private Long id;
    @ApiModelProperty("完成数量")
    private Integer completedCount;
    @ApiModelProperty("总数量")
    private Integer totalCount;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
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
