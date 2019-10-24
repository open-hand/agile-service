package io.choerodon.agile.infra.enums;

/**
 * 项目类别
 *
 * @author shinan.chen
 * @date 2019/03/13
 */
public class ProjectCategory {
    private ProjectCategory() {
    }

    /**
     * 敏捷项目
     */
    public static final String AGILE = "AGILE";
    /**
     * 普通项目组
     */
    public static final String PROGRAM = "PROGRAM";
    /**
     * 普通应用项目
     */
    public static final String GENERAL = "GENERAL";
    /**
     * 项目群子项目
     */
    public static final String PROGRAM_PROJECT = "PROGRAM_PROJECT";
}
