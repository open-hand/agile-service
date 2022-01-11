package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.DataLogFixVO;
import io.choerodon.agile.api.vo.DataLogQueryVO;
import io.choerodon.agile.api.vo.StatusVO;
import io.choerodon.agile.api.vo.business.AllDataLogVO;
import io.choerodon.agile.infra.dto.DataLogDTO;
import io.choerodon.agile.infra.dto.DataLogStatusChangeDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.ProductVersionDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;
import java.util.Set;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/6/14.
 * Email: fuqianghuang01@gmail.com
 */

public interface DataLogMapper extends BaseMapper<DataLogDTO> {

    List selectByIssueId(@Param("projectId") Long projectId,
                         @Param("issueId") Long issueId);

    DataLogDTO selectLastWorkLogById(@Param("projectId") Long projectId,
                                     @Param("issueId") Long issueId,
                                     @Param("field") String field);

    /**
     * 批量生成issue是否解决日志
     *
     * @param projectId    projectId
     * @param issueDTOS     issueDTOS
     * @param userId       userId
     * @param statusMapVO statusMapVO
     * @param completed    completed
     */
    void batchCreateStatusLogByIssueDOS(@Param("projectId") Long projectId, @Param("issueDTOS") List<IssueDTO> issueDTOS
            , @Param("userId") Long userId, @Param("statusMapVO") StatusVO statusMapVO, @Param("completed") Boolean completed);

    /**
     * 批量生成issue状态变更日志
     *
     * @param projectId projectId
     * @param issueDTOS  issueDTOS
     * @param userId    userId
     * @param oldStatus oldStatus
     * @param newStatus newStatus
     */
    void batchCreateChangeStatusLogByIssueDOS(@Param("projectId") Long projectId, @Param("issueDTOS") List<IssueDTO> issueDTOS, @Param("userId") Long userId,
                                              @Param("oldStatus") StatusVO oldStatus, @Param("newStatus") StatusVO newStatus);


    /**
     * 批量生成版本变更日志
     *
     * @param projectId        projectId
     * @param productVersionDTO productVersionDTO
     * @param issueIds         issueIds
     * @param userId           userId
     */
    void batchCreateVersionDataLog(@Param("projectId") Long projectId, @Param("productVersionDTO") ProductVersionDTO productVersionDTO, @Param("issueIds") List<Long> issueIds, @Param("userId") Long userId);

    /**
     * 批量删除错误数据
     *
     * @param dataLogIds dataLogIds
     */
    void batchDeleteErrorDataLog(@Param("dataLogIds") Set<Long> dataLogIds);

    /**
     * 更新脏数据
     *
     * @param dataLogStatusChangeDTOS dataLogStatusChangeDTOS
     */
    void batchUpdateErrorDataLog(@Param("dataLogStatusChangeDTOS") Set<DataLogStatusChangeDTO> dataLogStatusChangeDTOS);

    void batchCreateChangePriorityLogByIssueDOs(@Param("issueDTOS") List<IssueDTO> issueDTOS, @Param("userId") Long userId,
                                                @Param("oldPriorityName") String oldPriorityName, @Param("newPriorityName") String newPriorityName);

    List<DataLogFixVO> queryListByProjectId(Long projectId);

    List<DataLogDTO> selectResolutionIssueBySprint(@Param("projectId") Long projectId,
                                  @Param("sprintId") Long sprintId,
                                  @Param("field") String field,
                                  @Param("startDate") Date startDate,
                                  @Param("endDate") Date endDate);

    void updateProject(@Param("projectId") Long projectId, @Param("issueId") Long issueId, @Param("targetProjectId") Long targetProjectId);


    /**
     * 批量创建日志
     *
     * @param dataLogList dataLogList
     * @param userId           userId
     */
    void batchCreateDataLog(@Param("dataLogList") List<DataLogDTO> dataLogList, @Param("userId") Long userId);

    /**
     * 查询项目下问题操作历史
     * @param projectId 项目id
     * @param dataLogQueryVO 查询参数
     * @return 项目下问题操作历史
     */
    List<AllDataLogVO> listIssueDataLogByProjectId(@Param("projectId") Long projectId, @Param("dataLogQueryVO") DataLogQueryVO dataLogQueryVO);
}
