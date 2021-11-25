package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 11/25/21
 */
public class BoardQuickFilterRelVO {

    @Encrypt
    private Long id;
    @Encrypt
    private Long boardId;
    @Encrypt
    private Long quickFilterId;

    private Long projectId;

    private Long organizationId;

    private Long objectVersionNumber;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getBoardId() {
        return boardId;
    }

    public void setBoardId(Long boardId) {
        this.boardId = boardId;
    }

    public Long getQuickFilterId() {
        return quickFilterId;
    }

    public void setQuickFilterId(Long quickFilterId) {
        this.quickFilterId = quickFilterId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }
}
