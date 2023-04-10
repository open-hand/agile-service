package io.choerodon.agile.api.vo;

import java.util.Arrays;
import java.util.List;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author huaxin.deng@hand-china.com 2022/6/17
 */
public class IssueCopyLinkContents {

    public static final String ISSUE_LINKS = "issueLinks";
    public static final String ATTACHMENTS = "attachments";
    public static final String KNOWLEDGE_RELATIONS = "knowledgeRelations";
    public static final String PREDECESSORS = "predecessors";
    public static final String DECOMPOSE_BACKLOGS = "decomposeBacklogs";
    public static final String LINK_BACKLOGS = "linkBacklogs";
    public static final String RELATED_TEST_CASES = "relatedTestCases";
    public static final String RELATED_BRANCHES = "relatedBranches";
    public static final String COMMENTS = "comments";
    public static final List<String> ISSUE_COPY_LINK_CONTENTS = Arrays.asList(
        ISSUE_LINKS,
        ATTACHMENTS,
        KNOWLEDGE_RELATIONS,
        PREDECESSORS,
            DECOMPOSE_BACKLOGS,
        LINK_BACKLOGS,
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
    @ApiModelProperty(value = "来源需求")
    private boolean decomposeBacklogs;
    @ApiModelProperty(value = "关联需求")
    private boolean linkBacklogs;
    @ApiModelProperty(value = "关联测试用例")
    private boolean relatedTestCases;
    @ApiModelProperty(value = "关联分支")
    private boolean relatedBranches;
    @ApiModelProperty(value = "评论")
    private boolean comments;

    /**
     * @return 关联工作项
     */
    public boolean isIssueLinks() {
        return issueLinks;
    }

    public IssueCopyLinkContents setIssueLinks(boolean issueLinks) {
        this.issueLinks = issueLinks;
        return this;
    }

    /**
     * @return 附件
     */
    public boolean isAttachments() {
        return attachments;
    }

    public IssueCopyLinkContents setAttachments(boolean attachments) {
        this.attachments = attachments;
        return this;
    }

    /**
     * @return 关联知识
     */
    public boolean isKnowledgeRelations() {
        return knowledgeRelations;
    }

    public IssueCopyLinkContents setKnowledgeRelations(boolean knowledgeRelations) {
        this.knowledgeRelations = knowledgeRelations;
        return this;
    }

    /**
     * @return 前置依赖
     */
    public boolean isPredecessors() {
        return predecessors;
    }

    public IssueCopyLinkContents setPredecessors(boolean predecessors) {
        this.predecessors = predecessors;
        return this;
    }

    /**
     * @return 来源需求
     */
    public boolean isDecomposeBacklogs() {
        return decomposeBacklogs;
    }

    public IssueCopyLinkContents setDecomposeBacklogs(boolean decomposeBacklogs) {
        this.decomposeBacklogs = decomposeBacklogs;
        return this;
    }

    /**
     * @return 关联需求
     */
    public boolean isLinkBacklogs() {
        return linkBacklogs;
    }

    public IssueCopyLinkContents setLinkBacklogs(boolean linkBacklogs) {
        this.linkBacklogs = linkBacklogs;
        return this;
    }

    /**
     * @return 关联测试用例
     */
    public boolean isRelatedTestCases() {
        return relatedTestCases;
    }

    public IssueCopyLinkContents setRelatedTestCases(boolean relatedTestCases) {
        this.relatedTestCases = relatedTestCases;
        return this;
    }

    /**
     * @return 关联分支
     */
    public boolean isRelatedBranches() {
        return relatedBranches;
    }

    public IssueCopyLinkContents setRelatedBranches(boolean relatedBranches) {
        this.relatedBranches = relatedBranches;
        return this;
    }

    /**
     * @return 评论
     */
    public boolean isComments() {
        return comments;
    }

    public IssueCopyLinkContents setComments(boolean comments) {
        this.comments = comments;
        return this;
    }
}
