package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author zhaotianxin
 * @date 2021-03-25 14:36
 */
public interface BoardTemplateService {
    /**
     * 创建迭代计划模板
     * @param organizationId organizationId
     * @param boardName boardName
     */
    void createBoardTemplate(Long organizationId, String boardName);

    /**
     * 修改迭代模板
     * @param organizationId organizationId
     * @param boardId boardId
     * @param boardVO boardVO
     * @return result
     */
    BoardVO updateBoardTemplate(Long organizationId, Long boardId, BoardVO boardVO);

    /**
     * 删除迭代模板
     * @param organizationId organizationId
     * @param boardId boardId
     */
    void deleteBoardTemplate(Long organizationId, Long boardId);

    /**
     * 查询看板详情
     * @param organizationId organizationId
     * @param boardId boardId
     * @return result
     */
    BoardVO queryBoardTemplateById(Long organizationId, Long boardId);

    /**
     * 查询组织创建的看板模板
     * @param organizationId organizationId
     * @return result
     */
    Page<BoardVO> listBoardTemplate(Long organizationId, PageRequest pageRequest);

    BoardColumnVO createBoardColumn(Long organizationId, String categoryCode, BoardColumnVO boardColumnVO);

    BoardColumnVO updateBoardColumn(Long organizationId, Long columnId, Long boardId, BoardColumnVO boardColumnVO);

    BoardColumnVO updateColumnContraintTemplate(Long organizationId, Long columnId, ColumnWithMaxMinNumVO columnWithMaxMinNumVO);

    StatusVO moveStatusToUnCorrespond(Long organizationId, Long statusId, StatusMoveVO statusMoveVO);

    StatusVO moveStatusToColumn(Long organizationId, Long statusId, StatusMoveVO statusMoveVO);

    List<BoardColumnVO> listColumnByBoardId(Long organizationId, Long boardId);

    StatusTemplateVO settingStatusTemplate(Long organizationId, Long statusId, Boolean completed);

    void syncBoardTemplate(ProjectEvent projectEvent, String applyType);

    List<StatusVO> listUnCorrespondStatusTemplate(Long organizationId, Long boardTemplateId);

    void deleteBoardTemplateColumn(Long organizationId, Long templateColumnId);

    void deleteBoardTemplateStatus(Long currentStatusId, Long organizationId);
}
