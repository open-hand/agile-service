package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.ObjectSchemeFieldCreateVO;
import io.choerodon.agile.api.vo.UserVO;
import io.choerodon.agile.infra.dto.FileOperationHistoryDTO;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.List;
import java.util.Map;

/**
 * @author chihao.ran@hand-china.com
 * 2021/02/01 19:11
 */
public interface ObjectSchemeFieldExcelService {

    /**
     * 复制自定义字段导入模板
     * @param wb excel对象
     */
    void copyGuideSheetFromTemplate(Workbook wb);

    /**
     * 解析自定义字段导入模板标题列
     * @param sheet excel sheet
     * @param style  style
     */
    void generateHeaders(Sheet sheet, CellStyle style);

    /**
     * 填充自定义字段导入模板标题列
     * @param wb wb
     * @param sheet sheet
     * @param projectId 项目id
     * @param organizationId 组织id
     */
    void fillInPredefinedValues(Workbook wb, Sheet sheet, Long projectId, Long organizationId);

    /**
     * 校验导入值
     * @param workbook workbook
     * @param history history
     */
    boolean validExcelTemplate(Workbook workbook, FileOperationHistoryDTO history);

    /**
     * 获取自定义导入数据行数
     * @param sheet sheet
     * @return 行数
     */
    Integer getRealRowCount(Sheet sheet);

    /**
     * 获取用户名称map
     * @param organizationId 组织id
     * @param projectId 项目id
     * @return 用户名称map
     */
    Map<String, UserVO> getUserNameMap(Long organizationId, Long projectId);

    /**
     * 校验自定义字段导入是否已取消
     * @param organizationId 组织id
     * @param projectId 项目id
     * @param id 操作历史id
     * @param importedFieldIds 已导入字段的id
     * @return 是否已取消
     */
    boolean checkCanceled(Long organizationId, Long projectId, Long id, List<Long> importedFieldIds);

    /**
     * 是否跳过
     * @param row 行
     * @return 是否跳过
     */
    boolean isSkip(Row row);

    /**
     * 获取待创建的自定义字段
     * @param organizationId 组织id
     * @param projectId 项目id
     * @param row row
     * @param errorRowColMap 错误map
     * @param issueTypeNameMap 问题类型map
     * @return 待创建的自定义字段
     */
    ObjectSchemeFieldCreateVO generateObjectSchemeField(Long organizationId, Long projectId, Row row, Map<Integer, List<Integer>> errorRowColMap, Map<String, IssueTypeVO> issueTypeNameMap);

    /**
     * 是否为值列表导入
     * @param row 行
     * @return 是否为值列表导入
     */
    boolean isExtendKeyValue(Row row);

    /**
     * 设置值列表
     * @param objectSchemeFieldCreate 值列表关联的自定义字段
     * @param row 行
     */
    void setKeyValue(ObjectSchemeFieldCreateVO objectSchemeFieldCreate, Row row);

    /**
     * 校验值列表
     * @param objectSchemeFieldCreate  值列表关联的自定义字段
     * @param sheet sheet
     * @param r row
     * @param errorRowColMap 错误map
     * @return 校验结果
     */
    Map<String, Integer> validKeyValue(ObjectSchemeFieldCreateVO objectSchemeFieldCreate, Sheet sheet, int r, Map<Integer, List<Integer>> errorRowColMap);

    /**
     * 校验导入默认值
     * @param objectSchemeFieldCreate 默认值关联的自定义字段
     * @param keyRowMap 值列表map
     * @param row 行
     * @param userNameMap 用户名
     * @param errorRowColMap 错误map
     */
    void validAndSetDefaultValue(ObjectSchemeFieldCreateVO objectSchemeFieldCreate, Map<String, Integer> keyRowMap, Row row, Map<String, UserVO> userNameMap, Map<Integer, List<Integer>> errorRowColMap);

    /**
     * 创建自定义字段
     * @param projectId 项目id
     * @param organizationId 组织id
     * @param objectSchemeFieldCreate 创建的自定义字段
     * @param issueTypes 问题类型列表
     */
    void createObjectSchemeField(Long projectId, Long organizationId, ObjectSchemeFieldCreateVO objectSchemeFieldCreate, List<IssueTypeVO> issueTypes);

    /**
     * 设置导入错误结果excel并上传
     * @param errorRowColMap 错误
     * @param workbook workbook
     * @param sheet sheet
     * @param history 操作历史
     * @param organizationId 组织id
     */
    void generateErrorDataExcelAndUpload(Map<Integer, List<Integer>> errorRowColMap, Workbook workbook, Sheet sheet, FileOperationHistoryDTO history, Long organizationId);

    /**
     * 是否包含值列表
     * @param row 行
     * @return 是否包含值列表
     */
    // TODO 这个方法改名为hasKeyValue
    boolean isKeyValue(Row row);
}
