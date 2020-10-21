package io.choerodon.agile.app.service;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.PageConfigFieldVO;
import io.choerodon.agile.api.vo.QuickFilterValueVO;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.api.vo.business.IssueUpdateVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueSearchDTO;

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
     * @param issueType
     * @param pageFields
     * @return
     */
    List<PageFieldDTO> handlerProgramPageField(Long projectId, String issueType, List<PageFieldDTO> pageFields);

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
     * @param contextArray
     * @return
     */
    List<IssueTypeVO> queryProgramIssueType(Long projectId, List<IssueTypeVO> issueTypes, List<String> contextArray);

    /**
     * 项目群史诗查询pageConfig
     * @param projectId
     * @param issueType
     * @param pageConfigFieldVOS
     * @return
     */
    List<PageConfigFieldVO> queryProgramPageConfigFields(Long projectId, String issueType, List<PageConfigFieldVO> pageConfigFieldVOS);

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
}
