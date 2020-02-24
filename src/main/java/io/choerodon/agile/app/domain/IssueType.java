package io.choerodon.agile.app.domain;

/**
 * @author superlee
 * @since 2020-02-23
 */
public class IssueType {

    private Integer row;

    private String type;

    private IssueType next;

    public IssueType(Integer row, String type) {
        this.row = row;
        this.type = type;
    }

    public void setNext(IssueType next) {
        this.next = next;
    }

    public Integer getRow() {
        return row;
    }

    public String getType() {
        return type;
    }

    public IssueType getNext() {
        return next;
    }

    public Boolean hasNext() {
        return next != null;
    }
}
