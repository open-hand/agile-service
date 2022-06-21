package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.Arrays;
import java.util.List;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/6/17
 */
public class IssueCopyLinkContents {

    public static final String ISSUE_LINKS = "issueLinks";
    public static final String ATTACHMENTS = "attachments";
    public static final String KNOWLEDGE_RELATIONS = "knowledgeRelations";
    public static final String PREDECESSORS = "predecessors";
    public static final String RELATED_BACKLOGS = "relatedBacklogs";
    public static final String RELATED_TEST_CASES = "relatedTestCases";
    public static final String RELATED_BRANCHES = "relatedBranches";
    public static final String COMMENTS = "comments";
    public static final List<String> ISSUE_COPY_LINK_CONTENTS = Arrays.asList(
        ISSUE_LINKS,
        ATTACHMENTS,
        KNOWLEDGE_RELATIONS,
        PREDECESSORS,
        RELATED_BACKLOGS,
        RELATED_TEST_CASES,
        RELATED_BRANCHES,
        COMMENTS
    );

    @ApiModelProperty(value = "关联工作项")
    private boolean issueLinks;
    @ApiModelProperty(value = "附件")
    private boolean attachments;
    @ApiModelProperty(value = "关联知识")
    private boolean knowledgeRelations;
    @ApiModelProperty(value = "前置依赖")
    private boolean predecessors;
    @ApiModelProperty(value = "关联需求")
    private boolean relatedBacklogs;
    @ApiModelProperty(value = "关联测试用例")
    private boolean relatedTestCases;
    @ApiModelProperty(value = "关联分支")
    private boolean relatedBranches;
    @ApiModelProperty(value = "评论")
    private boolean comments;

    public boolean isIssueLinks() {
        return issueLinks;
    }

    public void setIssueLinks(boolean issueLinks) {
        this.issueLinks = issueLinks;
    }

    public boolean isAttachments() {
        return attachments;
    }

    public void setAttachments(boolean attachments) {
        this.attachments = attachments;
    }

    public boolean isKnowledgeRelations() {
        return knowledgeRelations;
    }

    public void setKnowledgeRelations(boolean knowledgeRelations) {
        this.knowledgeRelations = knowledgeRelations;
    }

    public boolean isPredecessors() {
        return predecessors;
    }

    public void setPredecessors(boolean predecessors) {
        this.predecessors = predecessors;
    }

    public boolean isRelatedBacklogs() {
        return relatedBacklogs;
    }

    public void setRelatedBacklogs(boolean relatedBacklogs) {
        this.relatedBacklogs = relatedBacklogs;
    }

    public boolean isRelatedTestCases() {
        return relatedTestCases;
    }

    public void setRelatedTestCases(boolean relatedTestCases) {
        this.relatedTestCases = relatedTestCases;
    }

    public boolean isRelatedBranches() {
        return relatedBranches;
    }

    public void setRelatedBranches(boolean relatedBranches) {
        this.relatedBranches = relatedBranches;
    }

    public boolean isComments() {
        return comments;
    }

    public void setComments(boolean comments) {
        this.comments = comments;
    }
}
