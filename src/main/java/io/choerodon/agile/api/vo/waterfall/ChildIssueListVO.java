package io.choerodon.agile.api.vo.waterfall;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.api.vo.IssueSubListVO;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/2/19
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ChildIssueListVO extends IssueSubListVO {
    @ApiModelProperty(value = "进度")
    private Integer progress;

    public Integer getProgress() {
        return progress;
    }

    public void setProgress(Integer progress) {
        this.progress = progress;
    }
}
