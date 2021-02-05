package io.choerodon.agile.app.service;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.*;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.*;

import java.util.List;
import java.util.Map;

/**
 * @author zhaotianxin
 * @date 2020-10-12 10:35
 */
public interface AgilePluginService {
    /**
     * 根据code判断问题类型code
     * @param code
     * @return
     */
    String getSystemFieldContext(String code);

    /**
     * 删除issue时,商业版要执行的逻辑
     * @param issueConvertDTO
     */
    void deleteIssueForBusiness(IssueConvertDTO issueConvertDTO);

    /**
     * 保存快速筛选处理商业版字段的sql
     * @param sqlQuery
     * @param quickFilterValueVO
     * @param value
     * @param operation
     * @param projectId
     */
    void appendProgramFieldSql(StringBuilder sqlQuery, QuickFilterValueVO quickFilterValueVO, String value, String operation,Long projectId);

    /**
     * 处理特性的rank值
     * @param projectId
     * @param type
     */
    void handlerFeatureRank(Long projectId, String type);

    /**
     * 查询项目群的史诗
     * @param epicIds
     * @param projectId
     */
    void getProgramEpicIds(List<Long> epicIds, Long projectId);

    /**
     * 过滤出项目群字段
     * @param projectId
     * @param issueTypeId
     * @param pageFields
     * @return
     */
    List<PageFieldDTO> handlerProgramPageField(Long projectId, Long issueTypeId, List<PageFieldDTO> pageFields);

    /**
     * 创建issue初始化特性相关的值
     * @param colorList
     * @param issueConvertDTO
     */
    void handleInitIssue(List<LookupValueDTO> colorList, IssueConvertDTO issueConvertDTO);

    /**
     * 修改issue时,如果是故事，关联的有特性，则冲刺也要关联特性
     * @param oldIssue
     * @param projectId
     * @param sprintId
     * @param issueType
     */
    void updateIssueSprintChanged(IssueConvertDTO oldIssue, Long projectId, Long sprintId, String issueType);

    /**
     * 修改issue,修改一些商业版属性的值
     * @param issueType
     * @param fieldList
     * @param projectId
     * @param issueUpdateVO
     * @param originIssue
     */
    void handlerProgramUpdateIssue(String issueType, List<String> fieldList, Long projectId, IssueUpdateVO issueUpdateVO, IssueDTO originIssue);

    /**
     * 修改issue时,校验特性
     * @param issueUpdateVO
     * @param projectId
     */
    void checkFeatureBeforeUpdateIssue(IssueUpdateVO issueUpdateVO, Long projectId);

    /**
     * issue批量移动到冲刺，如果关联特性要将特性和冲刺建立联系
     * @param projectId
     * @param sprintId
     * @param frontIncomingIssues
     * @param issueSearchDTOList
     */
    void handlerAssociateSprintsWithFeature(Long projectId, Long sprintId, List<Long> frontIncomingIssues, List<IssueSearchDTO> issueSearchDTOList);

    /**
     * 克隆issue时,克隆特性的特性价值以及验收标准等
     * @param issueId
     * @param issueCreateVO
     * @param applyType
     * @param projectId
     */
    void handlerCloneFeature(Long issueId, IssueCreateVO issueCreateVO, String applyType, Long projectId);

    /**
     * 查询issue详情时，设置商业版特有的属性值
     * @param issue
     */
    void setBusinessAttributes(IssueDetailDTO issue);

    /**
     * 对issueVO商业版的属性进行单独转换
     * @param issueVO
     * @param issue
     */
    void programIssueDetailDTOToVO(IssueVO issueVO,IssueDetailDTO issue);

    /**
     * 创建issue之前校验特性是否合法
     * @param issueCreateVO
     * @param applyType
     */
    void checkBeforeCreateIssue(IssueCreateVO issueCreateVO,String applyType);

    /**
     * 创建issue后对商业版特有属性进行单独赋值
     * @param issueConvertDTO
     * @param projectId
     * @param issueId
     * @param issueCreateVO
     */
    void handlerBusinessAfterCreateIssue(IssueConvertDTO issueConvertDTO, Long projectId, Long issueId, IssueCreateVO issueCreateVO);

    /**
     * 批量修改之前处理项目群的字段
     * @param projectId
     * @param predefinedFields
     * @param programMap
     * @param applyType
     */
    void handlerProgramPredefinedFields(Long projectId,JSONObject predefinedFields, Map<String, Object> programMap,String applyType);

    /**
     * 设置featureId
     * @param issueUpdateVO
     * @param programMap
     * @param fieldList
     */
    void setFeatureId(IssueUpdateVO issueUpdateVO, Map<String, Object> programMap,List<String> fieldList);

    /**
     * 批量修改特性的Pi、负责子团队以及冲刺
     * @param projectId
     * @param issueDTO
     * @param programMap
     */
    void handlerFeatureField(Long projectId, IssueDTO issueDTO, Map<String, Object> programMap);

    /**
     * 过滤项目群类型
     * @param projectId
     * @param typeWithValues
     * @return
     */
    List<LookupValueDTO> filterProgramType(Long projectId, LookupTypeWithValuesDTO typeWithValues);

    /**
     * 查询项目群的问题类型
     * @param projectId
     * @param issueTypes
     * @param issueTypeIds
     * @return
     */
    List<IssueTypeVO> queryProgramIssueType(Long projectId, List<IssueTypeVO> issueTypes, List<Long> issueTypeIds);

    /**
     * 项目群史诗查询pageConfig
     * @param projectId
     * @param issueTypeId
     * @param pageConfigFieldVOS
     * @return
     */
    List<PageConfigFieldVO> queryProgramPageConfigFields(Long projectId, Long issueTypeId, List<PageConfigFieldVO> pageConfigFieldVOS);

    /**
     * 添加项目群问题类型
     * @return
     */
    List<String> addProgramIssueType();

    /**
     * 对项目群史诗进行处理
     * @param objectSchemeFieldDTOS
     * @return
     */
    List<ObjectSchemeFieldDTO> filterProgramEpic(List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS);


    /**
     * 创建项目群时初始化项目群特有的问题类型和方案
     * @param projectEvent
     */
    void initProjectIssueTypeSchemeAndArt(ProjectEvent projectEvent);

    /**
     * 初始化项目群状态机
     * @param organizationId
     * @param projectEvent
     * @return
     */
    Long initPRStateMachine(Long organizationId, ProjectEvent projectEvent);

    /**
     * 查询项目群子项目故事地图
     * @param projectId
     * @param epicIds
     * @param searchVO
     * @return
     */
    StoryMapVO handlerBusinessQueryStoryMap(Long projectId, List<Long> epicIds, SearchVO searchVO);

    /**
     * 故事地图移动特性
     * @param projectId
     * @param storyMapDragVO
     */
    void handlerStoryMapMoveFeature(Long projectId, StoryMapDragVO storyMapDragVO);

    /**
     * 设置ApplyTypes
     * @param schemeVO
     * @param schemeId
     */
    void addApplyTypesToStateMachine(StateMachineSchemeVO schemeVO, Long schemeId);

    /**
     * 创建项目群子项目的冲刺
     * @param projectId
     * @param sprintConvertDTO
     */
    SprintConvertDTO createSubProjectSprint(Long projectId, SprintConvertDTO sprintConvertDTO);

    /**
     * 如果项目是项目群子项目,开启冲刺时设置开始时间为当前时间
     * @param projectId
     * @param sprintConvertDTO
     */
    void handlerSprintStartDate(Long projectId, SprintConvertDTO sprintConvertDTO);

    /**
     * 迭代计划添加商业版属性
     * @param projectId
     * @param issueIds
     * @param result
     */
    void addProgramAttr(Long projectId, List<Long> issueIds,Map<String, Object> result);

    /**
     * 查询单个冲刺的PI和类型
     * @param projectId
     * @param sprintId
     * @param sprintDetailVO
     * @return
     */
    SprintDetailVO setSprintPiAndType(Long projectId, Long sprintId, SprintDetailVO sprintDetailVO);

    /**
     * do 转issueListFieldKVDTOList 设置特性的属性
     * @param projectId
     * @param issueListFieldKVDTOList
     */
    void doToIssueListFieldKVDTO(Long projectId,List<IssueListFieldKVVO> issueListFieldKVDTOList);

    /**
     * 项目群子项目下载issue 替换史诗列为特性
     * @param copyFieldsName
     * @return
     */
    String[] changeFeatureHeaders(String[] copyFieldsName);

    List<SubFeatureVO> listFeature(Long organizationId, Long projectId);

    Map<String, Long> getFeatureMap(Long organizationId, Long projectId);

    /**
     * 删除子项目版本时，删除和项目群版本的关联关系
     * @param projectId
     * @param versionId
     */
    void deleteProgramVersionRel(Long projectId, Long versionId);

    List<IssueDTO> selectEpicBySubProjectFeature(Long subProjectId);

    void listStatusLinkageByStatusIds(Long projectId, Long issueTypeId, List<Long> statusIds, String applyType, List<StatusSettingVO> list);

    /**
     * 故事改变状态联动改变特性的状态
     * @param projectId
     * @param issueDTO
     * @param applyType
     */
    void storyLinkageFeature(Long projectId, IssueDTO issueDTO, String applyType);

    /**
     * 设置特性类型和团队信息
     * @param issues
     * @param organizationId
     */
    void setFeatureTypeAndFeatureTeams(List<IssueListFieldKVVO> issues,  Long organizationId);

    /**
     * 项目群子项目版本列表要返回关联的项目群版本信息
     * @param productVersionPageVOS
     * @param projectId
     * @param content
     */
    void settingProgramVersions(List<ProductVersionPageVO> productVersionPageVOS, Long projectId, List<Long> content);

    /**
     * 版本关联特性
     * @param programId
     * @param organizationId
     * @param featureIds
     * @param programVersionIds
     */
    void linkFeatureByBacklog(Long programId,Long organizationId, List<Long> featureIds, List<Long> programVersionIds);

    /**
     * 获取字段Code
     * @param fieldCodeS
     * @param issueTypeId
     */
    void getIssueTypeFieldCodes(List<String> fieldCodeS, Long issueTypeId);

    /**
     * 处理冲刺是否是规划中
     * @param projectId
     * @param list
     */
    void handlerSprintPlanInfo(Long projectId, List<SprintSearchVO> list);

    /**
     * 故事地图分页查询
     * @param projectId
     * @param epicIds
     * @param searchVO
     * @param page
     * @param size
     * @return
     */
    StoryMapVO handlerBusinessPageStoryMap(Long projectId, List<Long> epicIds, SearchVO searchVO, Integer page, Integer size);

    /**
     * 状态机流转处理特性的项目群版本
     * @param issueDTO
     * @param specifyMap
     */
    void handlerSpecifyProgramField(IssueDTO issueDTO, Map<String, Object> specifyMap);

    /**
     * 处理状态机自定义流转项目群字段属性
     *
     * @param statusFieldSettingVO
     * @param statusFieldValueSettingDTOS
     */
    void handlerProgramFieldValue(StatusFieldSettingVO statusFieldSettingVO, List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS);

    /**
     * 跨项目转交处理Feature需要清空得数据
     * @param projectId
     * @param issueDTO
     */
    void handlerFeatureCleanValue(Long projectId, IssueDetailDTO issueDTO);

    /**
     * 项目群子项目需要清空逻辑
     * @param issueDTO
     * @param issueUpdateVO
     * @param fieldList
     */
    void handlerFeatureSelfValue(IssueDetailDTO issueDTO, IssueUpdateVO issueUpdateVO, List<String> fieldList);

    /**
     * 修改agile_feature和wsjf表中数据的projectId
     * @param projectId
     * @param issueDTO
     * @param targetProjectId
     */
    void projectMoveUpdateFeatureValue(Long projectId, IssueDTO issueDTO, Long targetProjectId);

    /**
     * 设置商业版预定义字段的默认值对象
     * @param pageFieldViews
     * @param projectId
     * @param organizationId
     */
    void setBussinessDefaultValueObjs(List<PageFieldViewVO> pageFieldViews, Long projectId, Long organizationId);
}
