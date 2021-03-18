package io.choerodon.agile.infra.dto;

import java.io.InputStream;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/20 18:30
 */
public class StaticFileCompressDTO {
    private Long id;
    private InputStream in;
    private Integer size;
    private String prefixPath;
    private String fileName;
    private String status;
    private String issueId;
    private String encode;
    private StaticFileOperationHistoryDTO staticFileCompressHistory;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public InputStream getIn() {
        return in;
    }

    public void setIn(InputStream in) {
        this.in = in;
    }

    public Integer getSize() {
        return size;
    }

    public void setSize(Integer size) {
        this.size = size;
    }

    public String getPrefixPath() {
        return prefixPath;
    }

    public void setPrefixPath(String prefixPath) {
        this.prefixPath = prefixPath;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public StaticFileOperationHistoryDTO getStaticFileCompressHistory() {
        return staticFileCompressHistory;
    }

    public void setStaticFileCompressHistory(StaticFileOperationHistoryDTO staticFileCompressHistory) {
        this.staticFileCompressHistory = staticFileCompressHistory;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getIssueId() {
        return issueId;
    }

    public void setIssueId(String issueId) {
        this.issueId = issueId;
    }

    public String getEncode() {
        return encode;
    }

    public void setEncode(String encode) {
        this.encode = encode;
    }
}
