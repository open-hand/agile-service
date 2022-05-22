package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.utils.StringUtil;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author dinghuang123@gmail.com
 * @since 2019/1/23
 */
public class QuickFilterSearchVO {

    @ApiModelProperty(value = "搜索名称")
    private String filterName;

    @ApiModelProperty(value = "搜索内容")
    private List<String> contents;
    @Encrypt
    @ApiModelProperty(value = "快查id")
    private List<Long> quickFilterIds;
    @Encrypt
    @ApiModelProperty(value = "忽略的快查id")
    private List<Long> ignoredQuickFilterIds;

    public List<Long> getIgnoredQuickFilterIds() {
        return ignoredQuickFilterIds;
    }

    public void setIgnoredQuickFilterIds(List<Long> ignoredQuickFilterIds) {
        this.ignoredQuickFilterIds = ignoredQuickFilterIds;
    }

    public List<Long> getQuickFilterIds() {
        return quickFilterIds;
    }

    public void setQuickFilterIds(List<Long> quickFilterIds) {
        this.quickFilterIds = quickFilterIds;
    }

    public String getFilterName() {
        return filterName;
    }

    public void setFilterName(String filterName) {
        this.filterName = filterName;
    }

    public List<String> getContents() {
        return contents;
    }

    public void setContents(List<String> contents) {
        this.contents = contents;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }
}
