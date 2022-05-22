package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.springframework.util.ObjectUtils;

import java.util.List;
import java.util.Objects;

/**
 * @author superlee
 * @since 2021-11-17
 */
public class IssueTreeVO {
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "概要")
    private String summary;
    @ApiModelProperty(value = "问题编号")
    private String issueNum;
    @ApiModelProperty(value = "子级")
    private List<IssueTreeVO> children;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public String getIssueNum() {
        return issueNum;
    }

    public void setIssueNum(String issueNum) {
        this.issueNum = issueNum;
    }

    public List<IssueTreeVO> getChildren() {
        return children;
    }

    public void setChildren(List<IssueTreeVO> children) {
        this.children = children;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IssueTreeVO)) return false;
        IssueTreeVO that = (IssueTreeVO) o;
        return getIssueId().equals(that.getIssueId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getIssueId());
    }

    public String toString(int level, StringBuilder builder) {
        if (builder == null) {
            builder = new StringBuilder();
        }
        String suojin = "  ";
        for (int i = 0; i < level; i++) {
            builder.append(suojin);
        }
        builder
                .append(this.summary)
                .append("#")
                .append(this.issueNum)
                .append("#")
                .append(this.issueId)
                .append("\n");
        List<IssueTreeVO> children = this.getChildren();
        if (!ObjectUtils.isEmpty(children)) {
            level++;
            for (IssueTreeVO child : children) {
                child.toString(level, builder);
            }
        }
        return builder.toString();
    }
}
