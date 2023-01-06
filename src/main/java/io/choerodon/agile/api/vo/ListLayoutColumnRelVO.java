package io.choerodon.agile.api.vo;

import java.util.*;
import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModelProperty;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2021-05-07 14:13
 */
public class ListLayoutColumnRelVO {

    /**
     * 根据ColumnCode去重<br/>
     * 如有重复, 保留display=true OR sort更小的
     * @param input 输入
     * @return      输出
     */
    public static List<ListLayoutColumnRelVO> distinct(List<ListLayoutColumnRelVO> input) {
        if(CollectionUtils.isEmpty(input)) {
            return Collections.emptyList();
        }
        // 在HashMap的内存占用和效率之间找到最佳平衡点
        Map<String, ListLayoutColumnRelVO> distinctMap = new HashMap<>((int)Math.pow(2, (int)Math.ceil(Math.log(input.size())/Math.log(2))), 1);
        for (ListLayoutColumnRelVO inputRel : input) {
            String columnCode = inputRel.getColumnCode();
            if(StringUtils.isBlank(columnCode)) {
                continue;
            }
            ListLayoutColumnRelVO existsRel = distinctMap.get(columnCode);
            if(existsRel == null) {
                distinctMap.put(columnCode, inputRel);
            } else {
                // 当已存在的不展示且新输入展示, 则用新输入替换已存在的, continue
                if(!Boolean.TRUE.equals(existsRel.getDisplay()) && Boolean.TRUE.equals(inputRel.getDisplay())) {
                    distinctMap.put(columnCode, inputRel);
                    continue;
                }
                // 当已存在的sort大于新输入的sort, 则用新输入的替换已存在的, continue
                int existsRelSort = Optional.ofNullable(existsRel.getSort()).orElse(0);
                int inputRelSort = Optional.ofNullable(inputRel.getSort()).orElse(0);
                if(existsRelSort > inputRelSort) {
                    distinctMap.put(columnCode, inputRel);
                }
                // 否则, 丢弃新输入, 保留已存在的
            }
        }
        return new ArrayList<>(distinctMap.values());
    }

    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;
    @Encrypt
    @ApiModelProperty(value = "布局id")
    private Long layoutId;
    @Encrypt
    @ApiModelProperty(value = "字段id")
    private Long fieldId;
    @NotNull(message = "error.layout.column.code.null")
    @ApiModelProperty(value = "列编码")
    private String columnCode;
    @NotNull(message = "error.layout.column.width.null")
    @ApiModelProperty(value = "宽度")
    private Integer width;
    @NotNull(message = "error.layout.column.sort.null")
    @ApiModelProperty(value = "排序")
    private Integer sort;
    @ApiModelProperty(value = "是否展示")
    @NotNull(message = "error.layout.column.display.null")
    private Boolean display;
    @ApiModelProperty(value = "字段项目名称")
    private String fieldProjectName;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "额外配置：工时包含子任务")
    private Boolean extraConfig;

    public String getFieldProjectName() {
        return fieldProjectName;
    }

    public void setFieldProjectName(String fieldProjectName) {
        this.fieldProjectName = fieldProjectName;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getLayoutId() {
        return layoutId;
    }

    public void setLayoutId(Long layoutId) {
        this.layoutId = layoutId;
    }

    public String getColumnCode() {
        return columnCode;
    }

    public void setColumnCode(String columnCode) {
        this.columnCode = columnCode;
    }

    public Integer getWidth() {
        return width;
    }

    public void setWidth(Integer width) {
        this.width = width;
    }

    public Integer getSort() {
        return sort;
    }

    public void setSort(Integer sort) {
        this.sort = sort;
    }

    public Boolean getDisplay() {
        return display;
    }

    public void setDisplay(Boolean display) {
        this.display = display;
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

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public Boolean getExtraConfig() {
        return extraConfig;
    }

    public void setExtraConfig(Boolean extraConfig) {
        this.extraConfig = extraConfig;
    }
}
