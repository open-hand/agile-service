package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import org.hibernate.validator.constraints.NotBlank;
import io.choerodon.mybatis.domain.AuditDomain;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * 
 *
 * @author jiaxu.cui@hand-china.com 2020-09-23 09:29:15
 */
@ApiModel("页面规则字段")
@VersionAudit
@ModifyAudit
@Table(name = "agile_configuration_rule_filed")
public class ConfigurationRuleFiledDTO extends AuditDomain {

    public static final String FIELD_FIELD_CODE = "fieldCode";
    public static final String FIELD_TYPE = "type";
    public static final String FIELD_NAME = "name";
    public static final String FIELD_FIELD = "field";

//
// 业务方法(按public protected private顺序排列)
// ------------------------------------------------------------------------------

//
// 数据库字段
// ------------------------------------------------------------------------------

    @Id
    @GeneratedValue
    private String fieldCode;
    private String type;
    private String name;
    private String field;

//
// 非数据库字段
// ------------------------------------------------------------------------------

//
// getter/setter
// ------------------------------------------------------------------------------


    /**
     * @return 
     */
    public String getFieldCode() {
            return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
            this.fieldCode = fieldCode;
    }
    /**
     * @return type
     */
    public String getType() {
            return type;
    }

    public void setType(String type) {
            this.type = type;
    }
    /**
     * @return name
     */
    public String getName() {
            return name;
    }

    public void setName(String name) {
            this.name = name;
    }
    /**
     * @return field
     */
    public String getField() {
            return field;
    }

    public void setField(String field) {
            this.field = field;
    }
}
