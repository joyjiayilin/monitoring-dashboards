Data pipeline
================

## Purpose

This README documents the pipeline used to serve data to the
[Regional](https://github.com/joyjiayilin/monitoring-dashboards/tree/main/regional)
and [Executive
Summary](https://github.com/joyjiayilin/monitoring-dashboards/tree/main/exec)
Monitoring apps.

## Workflow

In general, this pipeline reads in data from Quest Analytics outputs
(.xlsx), makes necessary transformations, and uploads them to a set of
dedicated PostGRES tables.

![](Pipeline.png "Pipeline")
