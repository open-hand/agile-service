package io.choerodon.agile.api.vo;

/**
 * @author zhaotianxin
 * @date 2021-10-19 15:09
 */
public class ExcelTitleVO {
    private Long fieldId;

    private String title;

    private String code;

    private Integer width;

    private boolean number;

    public ExcelTitleVO() {
    }

    public ExcelTitleVO(String title, String code, Integer width) {
        this.title = title;
        this.code = code;
        this.width = width;
    }

    public ExcelTitleVO(String title, String code, Integer width, boolean number) {
        this.title = title;
        this.code = code;
        this.width = width;
        this.number = number;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public Integer getWidth() {
        return width;
    }

    public void setWidth(Integer width) {
        this.width = width;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public boolean getNumber() {
        return number;
    }

    public void setNumber(boolean number) {
        this.number = number;
    }
}
