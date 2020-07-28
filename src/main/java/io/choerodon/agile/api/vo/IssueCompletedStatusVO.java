package io.choerodon.agile.api.vo;

import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * @author jiaxu.cui@hand-china.com 2020/7/3 下午4:58
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class IssueCompletedStatusVO {

    public IssueCompletedStatusVO(String worker, Integer created) {
        this.worker = worker;
        this.created = created;
        this.completed = 0;
    }

    public IssueCompletedStatusVO(String worker) {
        this.worker = worker;
        this.created = 0;
        this.completed = 0;
    }

    public IssueCompletedStatusVO() {
    }

    private String worker;
    private Integer completed;
    private Integer created;

    public String getWorker() {
        return worker;
    }

    public void setWorker(String worker) {
        this.worker = worker;
    }

    public Integer getCompleted() {
        return completed;
    }

    public void setCompleted(Integer completed) {
        this.completed = completed;
    }

    public Integer getCreated() {
        return created;
    }

    public void setCreated(Integer created) {
        this.created = created;
    }
}
