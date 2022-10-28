package io.choerodon.agile.infra.dto;

/**
 * @author superlee
 * @since 2020-02-23
 */
public class IssueTypeLinkDTO {

    private Integer row;

    private String type;

    private Boolean son;

    private IssueTypeLinkDTO next;

    public IssueTypeLinkDTO(Integer row, String type) {
        this.row = row;
        this.type = type;
    }

    public void setNext(IssueTypeLinkDTO next) {
        this.next = next;
    }

    public Integer getRow() {
        return row;
    }

    public String getType() {
        return type;
    }

    public IssueTypeLinkDTO getNext() {
        return next;
    }

    public Boolean hasNext() {
        return next != null;
    }

    public void setSon(Boolean son) {
        this.son = son;
    }

    public Boolean getSon() {
        return son;
    }
}
