package io.choerodon.agile.app.service;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.event.StatusPayload;
import io.choerodon.agile.infra.dto.BoardDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */
public interface BoardService {

    void create(Long projectId, String boardName, String type);

    BoardVO update(Long projectId, Long boardId, BoardVO boardVO);

    void delete(Long projectId, Long boardId);

    BoardVO queryScrumBoardById(Long projectId, Long boardId);

    JSONObject queryAllData(Long projectId, Long boardId, Long organizationId, SearchVO searchVO);

    void initBoard(Long projectId, String boardName, List<StatusPayload> statusPayloads, String applyType);

    IssueMoveVO move(Long projectId, Long issueId, Long transformId, IssueMoveVO issueMoveVO, Boolean isDemo);

    List<BoardVO> queryByProjectId(Long projectId, String type);

    /**
     * 查询用户看板设置
     *
     * @param projectId projectId
     * @param boardId   boardId
     * @return UserSettingVO
     */
    UserSettingVO queryUserSettingBoard(Long projectId, Long boardId);

    /**
     * 更新用户swimLaneBasedCode设置
     *
     * @param projectId         projectId
     * @param boardId           boardId
     * @param swimlaneBasedCode swimlaneBasedCode
     * @return UserSettingVO
     */
    UserSettingVO updateUserSettingBoard(Long projectId, Long boardId, String swimlaneBasedCode);

    Boolean checkName(Long organizationId, Long projectId, String boardName);

    /**
     * 根据快速筛选id返回查询sql
     *
     * @param quickFilterIds
     * @return
     */
    String getQuickFilter(List<Long> quickFilterIds);

    /**
     * 根据个人查询id获取对象集合
     *
     * @param personFilterIds
     * @return
     */
    List<SearchVO> getSearchVO(List<Long> personFilterIds);

    /**
     * 判断issue拖动是否有状态联动
     *
     * @param projectId
     * @param issueId
     * @param statusId
     * @return
     */
    Boolean isLinked(Long projectId, Long issueId, Long statusId);

    BoardDTO createBoard(Long organizationId, Long projectId, String boardName, String type);

    Page<QuickFilterVO> pagedQueryQuickFilters(PageRequest pageRequest,
                                               Long projectId,
                                               Long boardId,
                                               QuickFilterSearchVO quickFilterSearchVO);

    void updateBoardQuickFilterRel(Long projectId,
                                   Long boardId,
                                   List<Long> quickFilterIds);

    List<BoardQuickFilterRelVO> listQuickFiltersByBoardId(Long projectId, Long boardId);
}
