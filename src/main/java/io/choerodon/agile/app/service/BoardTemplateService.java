package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.event.ProjectEvent;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-03-25 14:36
 */
public interface BoardTemplateService {
    /**
     * 创建迭代计划模板
     * @param organizationId
     * @param boardName
     */
    void createBoardTemplate(Long organizationId, String boardName);

    /**
     * 修改迭代模板
     * @param organizationId
     * @param boardId
     * @param boardVO
     * @return
     */
    BoardVO updateBoardTemplate(Long organizationId, Long boardId, BoardVO boardVO);

    /**
     * 删除迭代模板
     * @param organizationId
     * @param boardId
     */
    void deleteBoardTemplate(Long organizationId, Long boardId);

    /**
     * 查询看板详情
     * @param organizationId
     * @param boardId
     * @return
     */
    BoardVO queryBoardTemplateById(Long organizationId, Long boardId);

    /**
     * 查询组织创建的看板模板
     * @param organizationId
     * @return
     */
    List<BoardVO> listBoardTemplate(Long organizationId);

    BoardColumnVO createBoardColumn(Long organizationId, String categoryCode, BoardColumnVO boardColumnVO);

    BoardColumnVO updateBoardColumn(Long organizationId, Long columnId, Long boardId, BoardColumnVO boardColumnVO);

    BoardColumnVO updateColumnContraintTemplate(Long organizationId, Long columnId, ColumnWithMaxMinNumVO columnWithMaxMinNumVO);

    StatusVO moveStatusToUnCorrespond(Long organizationId, Long statusId, StatusMoveVO statusMoveVO);

    StatusVO moveStatusToColumn(Long organizationId, Long statusId, StatusMoveVO statusMoveVO);

    List<BoardColumnVO> listColumnByBoardId(Long organizationId, Long boardId);

    StatusTemplateVO settingStatusTemplate(Long organizationId, Long statusId, Boolean completed);

    void syncBoardTemplate(ProjectEvent projectEvent, String applyType);

    List<StatusVO> listUnCorrespondStatus(Long organizationId, Long boardTemplateId);
}
