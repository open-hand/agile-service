package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;
import java.util.Map;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/30 下午3:36
 */
public class IssueCountVO {

    /**
     * 创建list List<Map.Entry<用户名/时间，数量>>
     */
    @ApiModelProperty(value = "创建list List<Map.Entry<用户名/时间，数量>>")
    List<Map.Entry<String, Integer>> createdList;
    /**
     * 已解决(完成)list List<Map.Entry<用户名/时间，数量>>
     */
    @ApiModelProperty(value = "已解决(完成)list List<Map.Entry<用户名/时间，数量>>")
    List<Map.Entry<String, Integer>> completedList;

    public List<Map.Entry<String, Integer>> getCreatedList() {
        return createdList;
    }

    public void setCreatedList(List<Map.Entry<String, Integer>> createdList) {
        this.createdList = createdList;
    }

    public List<Map.Entry<String, Integer>> getCompletedList() {
        return completedList;
    }

    public void setCompletedList(List<Map.Entry<String, Integer>> completedList) {
        this.completedList = completedList;
    }
}
