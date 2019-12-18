package io.choerodon.agile.app.service;


import io.choerodon.agile.api.vo.IssueLabelVO;
import io.choerodon.agile.api.vo.LabelFixVO;
import io.choerodon.agile.infra.dto.IssueLabelDTO;

import java.util.List;

/**
 * 敏捷开发Issue标签
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:04:00
 */
public interface IssueLabelService {

    /**
     * 根据项目id查询issueLabel
     *
     * @param projectId projectId
     * @return IssueLabelVO
     */
    List<IssueLabelVO> listIssueLabel(Long projectId);

    /**
     * 添加一个敏捷开发Issue标签
     *
     * @param issueLabelDTO issueLabelDTO
     * @return IssueLabelE
     */
    IssueLabelDTO createBase(IssueLabelDTO issueLabelDTO);

    /**
     * 不是使用中的issue标签垃圾回收
     *
     * @param projectId projectId
     * @return int
     */
    int labelGarbageCollection(Long projectId);


    /**
     * 查询所有标签，数据迁移使用
     * @return
     */
    List<LabelFixVO> queryListByProjectId(Long projectId);
}