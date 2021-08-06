package io.choerodon.agile.api.vo.business;

import javax.validation.constraints.NotEmpty;

/**
 * @author superlee
 * @since 2021-08-02
 */
public class ConfigurationRuleCommentVO extends ConfigurationRuleSettingVO {

    @NotEmpty(message = "error.rule.comment.empty")
    private String comment;

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }
}
