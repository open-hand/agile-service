package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Set;

/**
 * @author zhaotianxin
 * @date 2021-10-14 14:25
 */
public class GanttChartSearchVO {

    @Encrypt
    private Set<Long> issueIds;

    private List<String> displayFieldCodes;

    public Set<Long> getIssueIds() {
        return issueIds;
    }

    public void setIssueIds(Set<Long> issueIds) {
        this.issueIds = issueIds;
    }

    public List<String> getDisplayFieldCodes() {
        return displayFieldCodes;
    }

    public void setDisplayFieldCodes(List<String> displayFieldCodes) {
        this.displayFieldCodes = displayFieldCodes;
    }
}
