package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.event.RemoveStatusWithProject;
import io.choerodon.agile.api.vo.event.StatusPayload;
import io.choerodon.agile.infra.dto.BoardColumnDTO;
import io.choerodon.agile.infra.dto.BoardDTO;
import io.choerodon.agile.infra.dto.ColumnWithStatusRelDTO;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */
public interface BoardColumnService {

    BoardColumnVO create(Long projectId, String categoryCode, String applyType, BoardColumnVO boardColumnVO);

    BoardColumnVO update(Long projectId, Long columnId, Long boardId, BoardColumnVO boardColumnVO);

    void delete(Long projectId, Long columnId);

    BoardColumnVO queryBoardColumnById(Long projectId, Long columnId);

    void initBoardColumns(Long projectId, Long boardId, List<StatusPayload> statusPayloads);

    void columnSort(Long organizationId, Long projectId, ColumnSortVO columnSortVO);

    void createColumnWithRelateStatus(BoardDTO boardResult);

    BoardColumnVO updateColumnContraint(Long projectId, Long columnId, ColumnWithMaxMinNumVO columnWithMaxMinNumVO);

    BoardColumnDTO createBase(BoardColumnDTO boardColumnDTO);

    void batchDeleteColumnAndStatusRel(List<RemoveStatusWithProject> removeStatusWithProjects);

    void relate(Long organizationId, Long projectId, Long boardId, String name, String categoryCode, Integer sequence, List<ColumnWithStatusRelDTO> columnWithStatusRelDTOList, String colorCode);

    void setColumnColor(BoardColumnVO boardColumnVO, Boolean checkStatus);

    Boolean checkColumnStatusExist(Long organizationId, Long projectId, Long statusId);

    void createCheck(BoardColumnVO boardColumnVO);

}
