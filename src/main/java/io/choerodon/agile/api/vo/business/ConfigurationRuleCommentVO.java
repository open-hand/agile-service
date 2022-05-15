package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;

import javax.validation.constraints.NotEmpty;
import java.util.Objects;

/**
 * @author superlee
 * @since 2021-08-02
 */
public class ConfigurationRuleCommentVO extends ConfigurationRuleSettingVO {

    @NotEmpty(message = "error.rule.comment.empty")
    @ApiModelProperty(value = "评论")
    private String comment;

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ConfigurationRuleCommentVO)) return false;
        ConfigurationRuleCommentVO that = (ConfigurationRuleCommentVO) o;
        return Objects.equals(getComment(), that.getComment());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getComment());
    }
}
