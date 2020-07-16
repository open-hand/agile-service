package io.choerodon.agile.api.vo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/22 下午3:07
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class KnowledgeRelationVO {

    private List<WikiRelationVO>  knowledgeRelationList;

    public List<WikiRelationVO> getKnowledgeRelationList() {
        return knowledgeRelationList;
    }

    public void setKnowledgeRelationList(List<WikiRelationVO> knowledgeRelationList) {
        this.knowledgeRelationList = knowledgeRelationList;
    }
}
