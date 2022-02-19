package io.choerodon.agile.api.vo.waterfall;

import io.choerodon.agile.api.vo.IssueSubListVO;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/2/19
 */
public class ChildIssueListVO extends IssueSubListVO {

    private Integer progress;

    public Integer getProgress() {
        return progress;
    }

    public void setProgress(Integer progress) {
        this.progress = progress;
    }
}
