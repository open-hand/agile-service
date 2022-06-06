package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import io.choerodon.agile.api.vo.business.*;
import org.apache.commons.collections.map.MultiKeyMap;
import org.apache.poi.ss.usermodel.Workbook;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.StarBeaconDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.domain.AuditDomain;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author zhaotianxin
 * @date 2020-09-22 16:27
 */
public interface BacklogExpandService {
    /**
     * 删除issue和backlog的关联关系
     *
     * @param issueId
     */
    void deleteIssueBacklogRel(Long issueId);

    /**
     * 自动变更backlog的状态
     *
     * @param issueId
     * @param projectId
     * @param organizationId
     */
    void changeDetection(Long issueId, Long projectId, Long organizationId);

    /**
     * 是否启用需求池
     *
     * @param projectId
     * @return
     */
    Boolean enabled(Long projectId);

    /**
     * 查询需求的页面字段
     *
     * @param issueType
     * @return
     */
    Map<String, PageConfigFieldEditedVO> fieldEdited(String issueType);

    /**
     * 返回需求的context
     *
     * @param code
     * @return
     */
    String getSystemFieldContext(String code);

    /**
     * 处理需求的字段
     *
     * @param editPageId
     * @param dataMap
     * @param rankMap
     * @param fields
     */
    void processBacklogFields(Long editPageId, MultiKeyMap dataMap, MultiKeyMap rankMap, List<ObjectSchemeFieldDTO> fields);

    /**
     * 初始化需求的maxNum
     *
     * @param projectId
     * @param maxNum
     */
    void initBacklogMaxNum(Long projectId, Long maxNum);

    /**
     * 获取需求字段与表名的映射
     *
     * @return list
     */
    List<FieldTableVO> getBacklogField();

    /**
     * 判断需求是否存在
     *
     * @param starBeaconDTO
     */
    void selectBacklogByStar(StarBeaconDTO starBeaconDTO);

    /**
     * 删除项目群版本和需求的关联关系
     *
     * @param programId
     * @param organizationId
     * @param programVersionId
     * @param targetProgramVersion
     */
    void deleteVersionBacklogRelByProgramVersionId(Long programId, Long organizationId, Long programVersionId, Long targetProgramVersion);

    /**
     * 发布项目群版本时,将关联的需求置为已发布
     *
     * @param programId
     * @param organizationId
     * @param programVersionId
     */
    void releaseProgramVersion(Long programId, Long organizationId, Long programVersionId);

    /**
     * 获取需求的字段code集合
     *
     * @param fieldCodes
     * @param issueTypeId
     */
    void getBacklogFieldCodes(List<String> fieldCodes, Long issueTypeId);

    Boolean checkFieldPageConfig(String issueType, String code, Boolean created, Boolean edited);

    /**
     * 设置需求预定义字段的默认值对象
     *
     * @param pageFieldViews
     * @param projectId
     * @param organizationId
     */
    void setBacklogDefaultValueObjs(List<PageFieldViewVO> pageFieldViews, Long projectId, Long organizationId);

    /**
     * 根据传入项目id判断是否开启了需求池
     *
     * @return 开启了需求池的项目id
     */
    Set<Long> listProjectIdsWhichEnableBacklog(Set<Long> projectIds);

    /**
     * 根据特性id集合，将特性关联的需求和版本进行关联
     *
     * @param issueIds
     * @param programVersionId
     */
    void associateWithBacklogAndVersionByFeatureIds(Set<Long> issueIds,
                                                    Long programVersionId,
                                                    Long organizationId,
                                                    Long programId);

    /**
     * 版本关联的需求写入excel
     *
     * @param workbook
     * @param sheetName
     * @param projectId
     * @param programVersionId
     * @param startRow
     * @return
     */
    int writeProgramVersionBacklog(Workbook workbook,
                                   String sheetName,
                                   Long projectId,
                                   Long programVersionId,
                                   int startRow);

    /**
     * 查询需求操作记录
     *
     * @param projectId      项目id
     * @param dataLogQueryVO 查询参数
     * @return 需求操作记录
     */
    List<AllDataLogVO> listBacklogDataLogByProjectId(Long projectId, DataLogQueryVO dataLogQueryVO);

    /**
     * 设置需求操作记录的需求信息
     *
     * @param backlogDataLog 需求操作记录
     */
    void setDataLogBacklogInfo(List<AllDataLogVO> backlogDataLog);

    Map<String, String> getUrlAndSummary(AuditDomain auditDomain, Long projectId);

    void addComment(AuditDomain auditDomain, String comment);

    boolean updateBacklogByField(List<ConfigurationRuleFieldVO> fields,
                                 Map<Long, ObjectSchemeFieldDTO> fieldMap,
                                 AuditDomain auditDomain,
                                 List<String> fieldList,
                                 List<PageFieldViewUpdateVO> customFields);

    Map<String, Long> filterRuleBySqlQuery(Long instanceId, Long projectId, List<ConfigurationRuleVO> ruleList);

    Map<Long, Map<String, String>> selectDataMapByIds(Set<Long> backlogIds);

    void sendUpdateBacklogMsg(Long backlogId,
                              List<String> fieldList,
                              List<PageFieldViewUpdateVO> customFields,
                              Long projectId);

    Map<Long, String> getOptionNameMapByOptionType(String optionType,
                                                   Long organizationId,
                                                   Long projectId);

    Page listBacklogFieldOption(String optionType,
                                Long organizationId,
                                Long projectId,
                                PageRequest pageRequest,
                                CascadeFieldOptionSearchVO cascadeFieldOptionSearchVO);

    List<IssueBacklogRelVO> selectBacklogRelByIssueIds(Long projectId, List<Long> issueIds);

    List<BacklogInfoVO> listBacklogByProjectIds(List<Long> projectIds);

    List<BacklogDataVO> listBacklogStatus(List<Long> backlogIds);

    List<BacklogInfoVO> listBacklogByIdsWithOption(List<Long> backlogIds, String param);
}
