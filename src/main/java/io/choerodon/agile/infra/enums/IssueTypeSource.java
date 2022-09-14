package io.choerodon.agile.infra.enums;

/**
 * 工作项类型来源
 * @author gaokuo.dai@zknow.com 2022-09-13
 */
public class IssueTypeSource {

    private IssueTypeSource() {
        throw new UnsupportedOperationException();
    }

    /**
     * 系统预制
     */
    public static final String SYSTEM = "system";

    /**
     * 组织创建
     */
    public static final String ORGANIZATION = "organization";

    /**
     * 项目创建
     */
    public static final String PROJECT = "project";

}
