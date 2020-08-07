package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.utils.StringUtil;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Map;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/31
 */
public class SearchVO {

    public void handleOtherArgs() {
        Map<String, Object> args = this.otherArgs;
        if (args != null) {
            List<String> list = (List<String>) args.get("sprint");
            if (list != null && list.contains("0")) {
                args.put("sprintNull", true);
            }
            list = (List<String>) args.get("version");
            if (list != null && list.contains("0")) {
                args.put("versionNull", true);
            }
            list = (List<String>) args.get("component");
            if (list != null && list.contains("0")) {
                args.put("componentNull", true);
            }
            list = (List<String>) args.get("epic");
            if (list != null && list.contains("0")) {
                args.put("epicNull", true);
            }
            list = (List<String>) args.get("label");
            if (list != null && list.contains("0")) {
                args.put("labelNull", true);
            }
            list = (List<String>) args.get("assigneeId");
            if (list != null && list.contains("0")) {
                args.put("assigneeIdNull", true);
            }
        }
    }


    private List<String> exportFieldCodes;
    /**
     * 输入查询参数
     */
    private Map<String, Object> searchArgs;

    /**
     * 过滤查询参数
     */
    private Map<String, Object> advancedSearchArgs;

    /**
     * 关联查询参数
     */
    private Map<String, Object> otherArgs;
    @Encrypt
    private List<Long> quickFilterIds;

    private List<Long> assigneeFilterIds;

    private Boolean onlyStory;

    private String content;

    private List<String> contents;

    public List<String> getExportFieldCodes() {
        return exportFieldCodes;
    }

    public void setExportFieldCodes(List<String> exportFieldCodes) {
        this.exportFieldCodes = exportFieldCodes;
    }

    public List<String> getContents() {
        return contents;
    }

    public void setContents(List<String> contents) {
        this.contents = contents;
    }

    public Boolean getOnlyStory() {
        return onlyStory;
    }

    public void setOnlyStory(Boolean onlyStory) {
        this.onlyStory = onlyStory;
    }

    public List<Long> getQuickFilterIds() {
        return quickFilterIds;
    }

    public void setQuickFilterIds(List<Long> quickFilterIds) {
        this.quickFilterIds = quickFilterIds;
    }

    public Map<String, Object> getSearchArgs() {
        return searchArgs;
    }

    public void setSearchArgs(Map<String, Object> searchArgs) {
        this.searchArgs = searchArgs;
    }

    public Map<String, Object> getAdvancedSearchArgs() {
        return advancedSearchArgs;
    }

    public void setAdvancedSearchArgs(Map<String, Object> advancedSearchArgs) {
        this.advancedSearchArgs = advancedSearchArgs;
    }

    public Map<String, Object> getOtherArgs() {
        return otherArgs;
    }

    public void setOtherArgs(Map<String, Object> otherArgs) {
        this.otherArgs = otherArgs;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public void setAssigneeFilterIds(List<Long> assigneeFilterIds) {
        this.assigneeFilterIds = assigneeFilterIds;
    }

    public List<Long> getAssigneeFilterIds() {
        return assigneeFilterIds;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }
}
