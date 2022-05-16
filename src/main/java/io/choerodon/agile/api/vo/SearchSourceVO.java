package io.choerodon.agile.api.vo;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.core.base.BaseConstants;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.format.annotation.DateTimeFormat;

/**
 * @author jiaxu.cui@hand-china.com 2020/7/23 下午7:59
 */
public class SearchSourceVO {
    @ApiModelProperty(value = "高级搜索参数")
    private AdvancedSearchArgs advancedSearchArgs;
    @ApiModelProperty(value = "其他搜索")
    private OtherArgs otherArgs;
    @ApiModelProperty(value = "搜索")
    private SearchArgs searchArgs;

    public class AdvancedSearchArgs{
        @Encrypt
        @ApiModelProperty(value = "问题类型id")
        private List<Long> issueTypeId;
        @Encrypt
        @ApiModelProperty(value = "状态id")
        private List<Long> statusId;
        @Encrypt
        @ApiModelProperty(value = "优先级id")
        private List<Long> priorityId;

        public List<Long> getIssueTypeId() {
            return issueTypeId;
        }

        public void setIssueTypeId(List<Long> issueTypeId) {
            this.issueTypeId = issueTypeId;
        }

        public List<Long> getStatusId() {
            return statusId;
        }

        public void setStatusId(List<Long> statusId) {
            this.statusId = statusId;
        }

        public List<Long> getPriorityId() {
            return priorityId;
        }

        public void setPriorityId(List<Long> priorityId) {
            this.priorityId = priorityId;
        }
    }

    public class OtherArgs{
        @Encrypt
        @ApiModelProperty(value = "经办人id")
        private List<Long> assigneeId;
        @Encrypt
        @ApiModelProperty(value = "史诗")
        private List<Long> epic;
        @Encrypt
        @ApiModelProperty(value = "冲刺")
        private List<Long> sprint;
        @Encrypt
        @ApiModelProperty(value = "版本")
        private List<Long> version;
        @Encrypt
        @ApiModelProperty(value = "模块")
        private List<Long> component;
        @ApiModelProperty(value = "自定义字段")
        private CustomField customField;

        public CustomField getCustomField() {
            return customField;
        }

        public void setCustomField(CustomField customField) {
            this.customField = customField;
        }

        public List<Long> getAssigneeId() {
            return assigneeId;
        }

        public void setAssigneeId(List<Long> assigneeId) {
            this.assigneeId = assigneeId;
        }

        public List<Long> getEpic() {
            return epic;
        }

        public void setEpic(List<Long> epic) {
            this.epic = epic;
        }

        public List<Long> getSprint() {
            return sprint;
        }

        public void setSprint(List<Long> sprint) {
            this.sprint = sprint;
        }

        public List<Long> getVersion() {
            return version;
        }

        public void setVersion(List<Long> version) {
            this.version = version;
        }

        public List<Long> getComponent() {
            return component;
        }

        public void setComponent(List<Long> component) {
            this.component = component;
        }

        public class CustomField{

            private Date date;
            private DateHms date_hms;
            private Number number;
            private Option option;
            private String string;
            private Text text;
             class Number{
                @Encrypt
                private Long fieldId;
                private Long value;

                public Long getFieldId() {
                    return fieldId;
                }

                public void setFieldId(Long fieldId) {
                    this.fieldId = fieldId;
                }

                public Long getValue() {
                    return value;
                }

                public void setValue(Long value) {
                    this.value = value;
                }
            }
            public class Date{
                @Encrypt
                private Long fieldId;
                private java.util.Date value;

                public Long getFieldId() {
                    return fieldId;
                }

                public void setFieldId(Long fieldId) {
                    this.fieldId = fieldId;
                }

                public java.util.Date getValue() {
                    return value;
                }

                public void setValue(java.util.Date value) {
                    this.value = value;
                }
            }
            public class DateHms{
                @Encrypt
                private Long fieldId;
                private java.util.Date value;

                public Long getFieldId() {
                    return fieldId;
                }

                public void setFieldId(Long fieldId) {
                    this.fieldId = fieldId;
                }

                public java.util.Date getValue() {
                    return value;
                }

                public void setValue(java.util.Date value) {
                    this.value = value;
                }
            }
            public class Option{
                @Encrypt
                private Long fieldId;
                private java.lang.String value;

                public Long getFieldId() {
                    return fieldId;
                }

                public void setFieldId(Long fieldId) {
                    this.fieldId = fieldId;
                }

                public java.lang.String getValue() {
                    return value;
                }

                public void setValue(java.lang.String value) {
                    this.value = value;
                }
            }
            public class String{
                @Encrypt
                private Long fieldId;
                private java.lang.String value;

                public Long getFieldId() {
                    return fieldId;
                }

                public void setFieldId(Long fieldId) {
                    this.fieldId = fieldId;
                }

                public java.lang.String getValue() {
                    return value;
                }

                public void setValue(java.lang.String value) {
                    this.value = value;
                }
            }
            public class Text{
                @Encrypt
                private Long fieldId;
                private java.lang.String value;

                public Long getFieldId() {
                    return fieldId;
                }

                public void setFieldId(Long fieldId) {
                    this.fieldId = fieldId;
                }

                public java.lang.String getValue() {
                    return value;
                }

                public void setValue(java.lang.String value) {
                    this.value = value;
                }
            }

            public Date getDate() {
                return date;
            }

            public void setDate(Date date) {
                this.date = date;
            }

            public DateHms getDate_hms() {
                return date_hms;
            }

            public void setDate_hms(DateHms date_hms) {
                this.date_hms = date_hms;
            }

            public Number getNumber() {
                return number;
            }

            public void setNumber(Number number) {
                this.number = number;
            }

            public Option getOption() {
                return option;
            }

            public void setOption(Option option) {
                this.option = option;
            }

            public String getString() {
                return string;
            }

            public void setString(String string) {
                this.string = string;
            }

            public Text getText() {
                return text;
            }

            public void setText(Text text) {
                this.text = text;
            }
        }
    }

    public class SearchArgs {
        @JsonFormat(pattern = BaseConstants.Pattern.DATETIME)
        @DateTimeFormat(pattern = BaseConstants.Pattern.DATETIME)
        private Date createEndDate;
        @JsonFormat(pattern = BaseConstants.Pattern.DATETIME)
        @DateTimeFormat(pattern = BaseConstants.Pattern.DATETIME)
        private Date createStartDate;
        @JsonFormat(pattern = BaseConstants.Pattern.DATETIME)
        @DateTimeFormat(pattern = BaseConstants.Pattern.DATETIME)
        private Date updateEndDate;
        @JsonFormat(pattern = BaseConstants.Pattern.DATETIME)
        @DateTimeFormat(pattern = BaseConstants.Pattern.DATETIME)
        private Date updateStartDate;

        public Date getCreateEndDate() {
            return createEndDate;
        }

        public void setCreateEndDate(Date createEndDate) {
            this.createEndDate = createEndDate;
        }

        public Date getCreateStartDate() {
            return createStartDate;
        }

        public void setCreateStartDate(Date createStartDate) {
            this.createStartDate = createStartDate;
        }

        public Date getUpdateEndDate() {
            return updateEndDate;
        }

        public void setUpdateEndDate(Date updateEndDate) {
            this.updateEndDate = updateEndDate;
        }

        public Date getUpdateStartDate() {
            return updateStartDate;
        }

        public void setUpdateStartDate(Date updateStartDate) {
            this.updateStartDate = updateStartDate;
        }

    }


    public AdvancedSearchArgs getAdvancedSearchArgs() {
        return advancedSearchArgs;
    }

    public void setAdvancedSearchArgs(AdvancedSearchArgs advancedSearchArgs) {
        this.advancedSearchArgs = advancedSearchArgs;
    }

    public OtherArgs getOtherArgs() {
        return otherArgs;
    }

    public void setOtherArgs(OtherArgs otherArgs) {
        this.otherArgs = otherArgs;
    }

    public SearchArgs getSearchArgs() {
        return searchArgs;
    }

    public void setSearchArgs(SearchArgs searchArgs) {
        this.searchArgs = searchArgs;
    }


}
